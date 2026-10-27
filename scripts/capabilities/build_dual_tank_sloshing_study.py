# ABOUTME: Build a source-neutral connected-tank analysis from public models.
"""Regenerate the connected-tank page using committed analytical sources and sanitized CFD evidence.

Run: python scripts/capabilities/build_dual_tank_sloshing_study.py
No solver, network access or third-party package is required. The model module
is loaded by file so optional hydrodynamics package dependencies are unnecessary.
"""
from __future__ import annotations

import ast
import csv
import hashlib
import io
import textwrap
import html
import importlib.util
import json
import math
from pathlib import Path
import re
import sys
import tomllib
from time import perf_counter

ROOT = Path(__file__).resolve().parents[2]
MODEL_PATH = ROOT / "src/digitalmodel/hydrodynamics/utube_tank.py"
TEST_PATH = ROOT / "tests/hydrodynamics/test_utube_tank.py"
REDUCTION_PATH = ROOT / "scripts/cfd/reduce_tld_two_peak.py"
SHOWCASE_PATH = ROOT / "scripts/capabilities/build_sloshing_cfd_showcase.py"
JSON_PATH = ROOT / "docs/api/structural/sloshing-tld-twopeak.json"
EXISTING = ROOT / "docs/api/cfd/sloshing-cfd-study.html"
OUTPUT = EXISTING.with_name("dual-tank-sloshing-cfd-study.html")
EVIDENCE_PATH = ROOT / "docs/api/cfd/data/dual_tank_cfd_evidence.json"
REPOSITORY_FILES = 'https://github.com/vamseeachanta/digitalmodel/blob/main/'

# Dates follow the commits that introduced each revision; related edits share a row.
REVISIONS = [
    ('Rev 1', '2026-10-23', 'Analytical scope, independent illustrative geometry and calibration limits introduced.'),
    ('Rev 2', '2026-10-23', 'Archived CFD comparisons and evidence limits added; damping labels and display precision clarified.'),
    ('Rev 3', '2026-10-25', 'Dimensioned schematic and interactive charts with CSV exports added; limitations consolidated.'),
    ('Rev 4', '2026-10-25', 'Engineering reporting conventions introduced with numbered sections, a source register and consistent plots.'),
    ('Rev 5', '2026-10-25', 'Illustrative and archived results separated; response terms and schematic dimensions clarified.'),
    ('Rev 6', '2026-10-25', 'Historical revisions restored and qualification linked from Appendix A; engineering results retained.'),
    ('Rev 7', '2026-10-25', 'References and schematic motion dimensions clarified; revision history placed in Appendix C.'),
    ('Rev 8', '2026-10-27', 'Definitions and calibration scope clarified; archived comparisons moved to an appendix; engineering results retained.'),
]


def revision_history():
    """Render the complete issue record and its recorded checking status."""
    rows = ''.join('<tr>' + ''.join(f'<td>{html.escape(value)}</td>' for value in
                   (*revision, 'issued for review')) + '</tr>'
                   for revision in REVISIONS)
    return ('<h2 id="revisions">Appendix C – Revision history</h2>'
            '<div id="revision-history"><table>'
            '<colgroup><col style="width:10%"/><col style="width:14%"/>'
            '<col style="width:52%"/><col style="width:24%"/></colgroup>'
            '<tr><th>Revision</th><th>Date</th>'
            '<th>Description</th><th>Status</th></tr>' + rows + '</table></div>')


def public_file_link(path):
    """Keep published assets relative and repository sources on the main branch."""
    relative = path.relative_to(ROOT).as_posix()
    if relative.startswith('docs/api/'):
        # Both data records are either alongside this page or one directory up.
        return ('../' + relative.removeprefix('docs/api/')
                if not relative.startswith('docs/api/cfd/') else
                relative.removeprefix('docs/api/cfd/'))
    return REPOSITORY_FILES + relative

# Independent round inputs; transfer of the reference calibration is assumed.
ILLUSTRATIVE_LEG_LENGTH = 30.0
ILLUSTRATIVE_LEG_WIDTH = 8.0
ILLUSTRATIVE_FILL_DEPTH = 4.0
ILLUSTRATIVE_CENTROID_OFFSET = 10.0
ILLUSTRATIVE_CONDUIT_AREA = 8.0
FROZEN_VOLUME_INERTIA_INDEX = 2  # _prismatic_properties returns volume inertia here.


def load_module(name, path):
    spec = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(spec)
    sys.modules[name] = module
    spec.loader.exec_module(module)
    return module


MODEL = load_module("_dual_tank_public_model", MODEL_PATH)
REDUCTION = load_module("_dual_tank_public_reduction", REDUCTION_PATH)
SHOWCASE = load_module("_dual_tank_public_showcase", SHOWCASE_PATH)


def published_claims():
    """Read explicit published statements; fail if their source wording changes."""
    doc = MODEL.__doc__
    tests = TEST_PATH.read_text(encoding="utf-8")
    return {
        "moment_rms": float(re.search(r"has ([\d.]+) percent RMS", doc)[1]),
        "moment_min_period": int(re.search(r"periods of (\d+) seconds", doc)[1]),
        "degrades_below": int(re.search(r"Below about (\d+) seconds", doc)[1]),
        "length_inflation": float(re.search(r"length by ([\d.]+) times", doc)[1]),
        "cases": int(re.search(r"Published (\d+)-case fit outputs", tests)[1]),
        "area_exponent": MODEL.DEFAULT_AREA_EXPONENT,
        "loss_rms": MODEL.CALIBRATED_AREA_EXPONENT_RMS_PERCENT,
        "linear_rms": MODEL.LINEAR_AREA_EXPONENT_RMS_PERCENT,
    }


def test_inputs():
    """Read published test inputs without executing tests or copying measurements."""
    tree = ast.parse(TEST_PATH.read_text(encoding="utf-8"))
    constants = {
        node.targets[0].id: ast.literal_eval(node.value)
        for node in tree.body
        if isinstance(node, ast.Assign) and isinstance(node.value, ast.Constant)
    }
    functions = {n.name: n for n in tree.body if isinstance(n, ast.FunctionDef)}

    def keyword(function, call, key):
        for node in ast.walk(functions[function]):
            if isinstance(node, ast.Call):
                name = getattr(node.func, "id", getattr(node.func, "attr", ""))
                if name == call:
                    for kw in node.keywords:
                        if kw.arg == key:
                            return ast.literal_eval(kw.value)
        raise ValueError(f"Missing public input: {function}:{key}")

    def rows(function):
        decorator = functions[function].decorator_list[0]
        return ast.literal_eval(decorator.args[1])

    common = "test_tank_reduces_peak_roll_near_resonance"
    damping = "test_equivalent_damping_rises_with_roll_amplitude"
    levels = [
        ast.literal_eval(kw.value)
        for node in ast.walk(functions[damping]) if isinstance(node, ast.Call)
        for kw in node.keywords if kw.arg == "level_difference_amplitude"
    ]
    moment_fn = functions["test_tank_moment_components_match_measurements"]
    roll = next(
        ast.literal_eval(n.args[0]) for n in ast.walk(moment_fn)
        if isinstance(n, ast.Call) and getattr(n.func, "attr", "") == "radians"
    )
    return {
        "constants": constants,
        "areas": [r[0] for r in rows("test_natural_period_matches_cfd")],
        "max_period": max(r[0] for r in rows(moment_fn.name)),
        "vessel_period": keyword(common, "dict", "vessel_roll_period"),
        "hull_damping": keyword(common, "dict", "hull_damping_ratio"),
        "authority": keyword(common, "dict", "tank_authority"),
        "levels": levels,
        "damping_period": keyword(damping, "dict", "forcing_period"),
        "roll_degrees": roll,
    }


def grid(low, high, count):
    return [low + (high - low) * i / (count - 1) for i in range(count)]


def compute():
    inputs = test_inputs()
    manifest = json.loads(JSON_PATH.read_text(encoding="utf-8"))
    claims = published_claims()
    geometry = MODEL.UTubeGeometry(
        leg_length=ILLUSTRATIVE_LEG_LENGTH,
        leg_width=ILLUSTRATIVE_LEG_WIDTH,
        fill_depth=ILLUSTRATIVE_FILL_DEPTH,
        centroid_offset=ILLUSTRATIVE_CENTROID_OFFSET,
    )
    area = ILLUSTRATIVE_CONDUIT_AREA
    count = len(manifest["frequency_ratio_grid"])
    areas = grid(min(inputs["areas"]), max(inputs["areas"]), count)
    periods = grid(claims["degrades_below"], inputs["max_period"], count)
    natural = [MODEL.natural_period(
        leg_area=geometry.leg_area, conduit_area=a, fill_depth=geometry.fill_depth,
        effective_conduit_length=geometry.effective_conduit_length,
    ) for a in areas]
    inertia = [
        MODEL.DEFAULT_DENSITY
        * MODEL._prismatic_properties(geometry, a)[FROZEN_VOLUME_INERTIA_INDEX]
        for a in areas
    ]
    damping = [[MODEL.equivalent_damping(
        geometry=geometry, conduit_area=a, forcing_period=inputs["damping_period"],
        level_difference_amplitude=level,
    ) for a in areas] for level in inputs["levels"]]
    moments = [MODEL.tank_moment(
        geometry=geometry, conduit_area=area, forcing_period=period,
        roll_amplitude=math.radians(inputs["roll_degrees"]),
    ) for period in periods]
    responses = [[abs(MODEL.coupled_roll_response(
        forcing_period=period, vessel_roll_period=inputs["vessel_period"],
        hull_damping_ratio=inputs["hull_damping"], tank_authority=inputs["authority"],
        geometry=geometry, conduit_area=area, tank_enabled=enabled,
    )) for period in periods] for enabled in (False, True)]
    gs = manifest["frequency_ratio_grid"]
    mu = manifest["meta"]["mass_ratio_mu"]
    tuning = manifest["meta"]["tuning_ratio_f"]
    absorbers = []
    for curve in manifest["curves"]:
        ys = [REDUCTION._response(g, mu, tuning, curve["zeta"]) for g in gs]
        absorbers.append({
            "ys": ys, "published": curve["response"], "zeta": curve["zeta"],
            "peaks": REDUCTION._analyse(gs, ys)["n_peaks"],
        })
    return dict(inputs=inputs, geometry=geometry, area=area, areas=areas,
                periods=periods, natural=natural, inertia=inertia, damping=damping,
                moments=moments, responses=responses, gs=gs, mu=mu,
                tuning=tuning, absorbers=absorbers)


PLOT_LEFT, PLOT_RIGHT = 72, 342
PLOT_TOP, PLOT_BOTTOM = 32, 252
SERIES_COLORS = ('var(--navy)', 'var(--teal)', 'var(--accent)', 'var(--mut)', 'var(--red)')


def nice_step(value):
    """Round an axis step upward to a power-of-ten multiple of 1, 2 or 5."""
    power = 10 ** math.floor(math.log10(value))
    return next(multiplier * power for multiplier in (1, 2, 5, 10)
                if multiplier * power >= value * (1 - 1e-12))


def axis_range(values, zero=True):
    low, high = min(values), max(values)
    if zero:
        low = min(0, low)
    step = nice_step((high - low or 1) / 4)
    return (math.floor(low / step) * step, math.ceil(high / step) * step), step


def source_pointer(fact):
    source = fact['provenance']
    return source['path'] + '#' + source['pointer']


def chart_specs(data=None, evidence=None, evidence_only=False):
    """Define CSV rows and SVG series together from the analytical and evidence inputs."""
    d = (compute() if data is None else data) if not evidence_only else None
    e = json.loads(EVIDENCE_PATH.read_text(encoding='utf-8')) if evidence is None else evidence
    val = lambda fact: fact['value']
    specs = []

    def add(identifier, title, headers, rows, xkey, names, xlabel, ylabel,
            xunit, yunit, sources, continuous=False, **options):
        # Model evaluations have no archived decimal precision. Twelve significant
        # digits remove interpreter-dependent floating-point tails in exports;
        # archived observations and their provenance remain untouched.
        if continuous:
            rows = [[float(format(value, '.12g')) if isinstance(value, float) else value
                     for value in row] for row in rows]
        series = []
        for index, (name, ykey) in enumerate(names):
            points = []
            for row_index, row in enumerate(rows):
                mapped = dict(zip(headers, row))
                if mapped[ykey] is None:
                    continue
                source = sources[index][row_index] if isinstance(sources[index], list) else sources[index]
                points.append(dict(x=mapped[xkey], y=mapped[ykey], row=row_index, source=source))
            series.append(dict(name=name, xkey=xkey, ykey=ykey, points=points))
        xrange, xstep = axis_range([row[headers.index(xkey)] for row in rows], zero=False)
        yrange, ystep = axis_range([p['y'] for s in series for p in s['points']])
        specs.append(dict(id=identifier, title=title, headers=headers, rows=rows,
                          series=series, xlabel=xlabel, ylabel=ylabel, xunit=xunit,
                          yunit=yunit, continuous=continuous, xrange=xrange, xstep=xstep,
                          yrange=yrange, ystep=ystep, **options))
        return specs[-1]

    model_source = MODEL_PATH.relative_to(ROOT).as_posix()
    inputs_source = TEST_PATH.relative_to(ROOT).as_posix()
    response_source = model_source + '#coupled_roll_response; ' + inputs_source + '#test_tank_reduces_peak_roll_near_resonance'
    if d is not None:
        c1 = add('coupled-response',
                 'Predicted roll response with and without the connected tank (illustrative system)',
                 ['forcing_period_s', 'roll_bare_per_unit_moment', 'roll_with_tank_per_unit_moment'],
                 [list(row) for row in zip(d['periods'], *d['responses'])], 'forcing_period_s',
                 [('Bare vessel', 'roll_bare_per_unit_moment'), ('With connected tank', 'roll_with_tank_per_unit_moment')],
                 'Forcing period (s)', 'Roll per unit wave moment (dimensionless)',
                 's', 'dimensionless', [response_source] * 2, True,
                 validity=published_claims()['moment_min_period'])
        c1.update(xrange=(min(d['periods']), max(d['periods'])), xstep=10)

    calibration = e['calibration']
    checks = e['period_checks']
    historical = {val(r['area']): r['historical_period'] for r in e['area_sweep']}
    area_checks = [r for r in checks if val(r['fill']) == val(calibration['fill'])]
    cases = sorted([*area_checks, calibration], key=lambda r: val(r['area']))
    rows, pointers = [], []
    for case in cases:
        area = val(case['area'])
        role = 'calibration reference' if case is calibration else 'independent check'
        facts = [case['measured_period'], case['predicted_period']]
        if area in historical:
            facts.append(historical[area])
        pointer = ' | '.join(source_pointer(fact) for fact in facts)
        rows.append([area, val(case['fill']), role, val(case['measured_period']),
                     val(case['predicted_period']), val(historical[area]) if area in historical else None, pointer])
        pointers.append(pointer)
    c2 = add('natural-period-vs-area',
             'Exchange natural period against conduit area (archived connected-tank CFD, 50 % fill)',
             ['area_m2', 'fill_pct', 'role', 'cfd_period_s', 'model_period_s', 'historical_period_s', 'evidence_pointer'],
             rows, 'area_m2', [('CFD-derived period', 'cfd_period_s'),
                               ('Calibration reference', 'cfd_period_s'),
                               ('Calibrated model', 'model_period_s'),
                               ('Historical geometric-length model', 'historical_period_s')],
             'Conduit area (m²)', 'Exchange natural period (s)', 'm²', 's', [pointers] * 4,
             tolerance=val(e['published_band']), band_source=source_pointer(e['published_band']))
    c2.update(xrange=(0, 15), xstep=3, yrange=(8, 28), ystep=4)
    for series, role in zip(c2['series'][:2], ('independent check', 'calibration reference')):
        series['points'] = [point for point in series['points'] if rows[point['row']][2] == role]
    c2['series'][2]['shape'] = 'diamond'
    c2['series'][3]['shape'] = 'cross'

    band = val(e['published_band'])
    c3 = add('period-error', 'Calibrated period error against the reference ±3 % band',
             ['area_m2', 'fill_pct', 'predicted_period_s', 'cfd_period_s', 'error_pct', 'band_pct', 'inside_band', 'evidence_pointer'],
             [[val(r['area']), val(r['fill']), val(r['predicted_period']), val(r['measured_period']),
               val(r['error']), band, abs(val(r['error'])) <= band,
               ' | '.join(source_pointer(r[k]) for k in ('error', 'predicted_period', 'measured_period'))] for r in checks],
             'area_m2', [('Prediction error', 'error_pct')], 'Case (m², %)', 'Period error (%)',
             'm²', '%', [[source_pointer(r['error']) for r in checks]],
             categories=[f"{val(r['area']):.1f} m², {val(r['fill']):.0f} %" for r in checks],
             tolerance=band, band_source=source_pointer(e['published_band']))
    c3.update(yrange=(-5, 5), ystep=1)

    if d is not None:
        add('natural', 'Rigid-column natural period against conduit area (illustrative system)',
            ['area_m2', 'natural_period_s'], [list(r) for r in zip(d['areas'], d['natural'])],
            'area_m2', [('Rigid-column model', 'natural_period_s')],
            'Conduit area (m²)', 'Period (s)', 'm²', 's', [model_source + '#natural_period'], True)
        add('inertia', 'Frozen-fluid roll inertia against conduit area (illustrative system)',
            ['area_m2', 'inertia_million_kg_m2'],
            [[a, v / 1e6] for a, v in zip(d['areas'], d['inertia'])],
            'area_m2', [('Frozen-fluid prismatic model', 'inertia_million_kg_m2')],
            'Conduit area (m²)', 'Inertia (million kg m²)', 'm²', 'million kg m²',
            [model_source + '#_prismatic_properties'], True)
        headers = ['area_m2'] + [f'damping_level_{i}_pct' for i in range(len(d['damping']))]
        add('damping', 'Quadratic-loss damping against conduit area (illustrative system)', headers,
            [list(r) for r in zip(d['areas'], *[[v * 100 for v in ys] for ys in d['damping']])],
            'area_m2', [(f'Level-difference amplitude {level:.1f} m', headers[i + 1])
                        for i, level in enumerate(d['inputs']['levels'])],
            'Conduit area (m²)', 'Damping ratio (%)', 'm²', '%',
            [model_source + '#equivalent_damping'] * len(d['damping']), True)
        add('moment', 'Tank-on-vessel moment against forcing period (illustrative system)',
            ['forcing_period_s', 'in_phase_MN_m', 'quadrature_MN_m', 'magnitude_MN_m'],
            [[t, m.in_phase / 1e6, m.quadrature / 1e6, m.magnitude / 1e6]
             for t, m in zip(d['periods'], d['moments'])], 'forcing_period_s',
            [('In phase', 'in_phase_MN_m'), ('Quadrature', 'quadrature_MN_m'), ('Magnitude', 'magnitude_MN_m')],
            'Forcing period (s)', 'Moment (MN m)', 's', 'MN m', [model_source + '#tank_moment'] * 3,
            True, validity=published_claims()['moment_min_period'])
        headers = ['frequency_ratio_no_units'] + [f'roll_damping_{i}_no_units' for i in range(len(d['absorbers']))]
        add('split', 'Single-tank tuned absorber (comparison model)', headers,
            [list(r) for r in zip(d['gs'], *[a['ys'] for a in d['absorbers']])], headers[0],
            [(f"Damping {a['zeta'] * 100:.1f} %", headers[i + 1]) for i, a in enumerate(d['absorbers'])],
            'Forcing / vessel natural frequency (dimensionless)', 'Roll / static deflection (dimensionless)',
            'dimensionless', 'dimensionless', [REDUCTION_PATH.relative_to(ROOT).as_posix() + '#_response; ' +
                                    JSON_PATH.relative_to(ROOT).as_posix() + f'#/curves/{i}'
                                    for i in range(len(d['absorbers']))], True)

    def evidence_chart(identifier, title, cases, xfield, yfields, xlabel, ylabel, xunit, yunit):
        headers = [xfield[1]] + [field[1] for field in yfields] + ['evidence_pointer']
        rows = [[val(r[xfield[0]])] + [val(r[field[0]]) for field in yfields] +
                [' | '.join(source_pointer(r[field[0]]) for field in yfields)] for r in cases]
        return add(identifier, title, headers, rows, xfield[1],
                   [(field[2], field[1]) for field in yfields], xlabel, ylabel, xunit, yunit,
                   [[source_pointer(r[field[0]]) for r in cases] for field in yfields])

    evidence_chart('cfd-amplitude', 'Exchange amplitude against conduit area (archived CFD geometry)',
                   e['area_sweep'], ('area', 'area_m2'),
                   [('cfd_amplitude', 'cfd_amplitude_m3', 'CFD'),
                    ('analytical_amplitude', 'historical_amplitude_m3', 'Historical analytical')],
                   'Conduit area (m²)', 'Exchange amplitude (m³)', 'm²', 'm³')
    fill = sorted([r for r in checks if val(r['fill']) != val(calibration['fill'])
                   and val(r['area']) == val(calibration['area'])], key=lambda r: val(r['fill']))
    evidence_chart('cfd-fill-period', 'Natural period against fill (archived CFD geometry)', fill,
                   ('fill', 'fill_pct'), [('measured_period', 'cfd_period_s', 'CFD-derived'),
                                          ('predicted_period', 'model_period_s', 'Predicted')],
                   'Fill (%)', 'Natural period (s)', '%', 's')
    chosen = [dict(fill=r['fill'], **min(r['damping'], key=lambda c:
                  (val(c['roll']), abs(val(c['period']) - val(r['predicted_period']))))) for r in fill]
    evidence_chart('cfd-fill-damping', 'Equivalent damping against fill (archived CFD geometry)', chosen,
                   ('fill', 'fill_pct'), [('measured', 'cfd_damping_no_units', 'CFD-derived'),
                                        ('linear_prediction', 'inverse_area_damping_no_units', 'Original inverse-area prediction')],
                   'Fill (%)', 'Damping ratio (dimensionless)', '%', 'dimensionless')
    loss = sorted(area_checks, key=lambda r: val(r['area']))
    near = [dict(area=r['area'], **min(r['damping'], key=lambda c:
                abs(val(c['period']) - val(r['predicted_period'])))) for r in loss]
    evidence_chart('cfd-loss-area', 'Equivalent damping against conduit area (archived CFD geometry)', near,
                   ('area', 'area_m2'), [('measured', 'cfd_damping_no_units', 'CFD-derived, near predicted natural period'),
                                        ('linear_prediction', 'inverse_area_damping_no_units', 'Original inverse-area prediction')],
                   'Conduit area (m²)', 'Damping ratio (dimensionless)', 'm²', 'dimensionless')
    evidence_chart('cfd-forced-exchange', 'Medium-mesh forced exchange (archived CFD geometry)', e['forced_sweep'],
                   ('period', 'forcing_period_s'), [('exchange', 'exchange_amplitude_m3', 'Exchange amplitude')],
                   'Forcing period (s)', 'Exchange amplitude (m³)', 's', 'm³')
    return specs


def csv_text(spec):
    """Keep the exact source precision and use UTF-8-compatible LF rows."""
    stream = io.StringIO(newline='')
    writer = csv.writer(stream, lineterminator='\n')
    writer.writerow(spec['headers'])
    writer.writerows(spec['rows'])
    return stream.getvalue()


def svg_text(x, y, value, **attrs):
    """Wrap long SVG labels at a legible size in the narrowest layout."""
    lines = textwrap.wrap(value, width=attrs.pop('wrap', 38)) or ['']
    attributes = ' '.join(f'{key.rstrip("_").replace("_", "-")}="{html.escape(str(value), quote=True)}"'
                          for key, value in attrs.items())
    spans = ''.join(f'<tspan x="{x}" dy="{0 if i == 0 else 16}">{html.escape(line)} </tspan>'
                    for i, line in enumerate(lines))
    return f'<text x="{x}" y="{y}" {attributes}>{spans}</text>'


def marker(shape, x, y, color):
    attrs = f'stroke="{color}" stroke-width="1"'
    if shape == 'diamond':
        return f'<polygon points="{x:.2f},{y-3:.2f} {x+3:.2f},{y:.2f} {x:.2f},{y+3:.2f} {x-3:.2f},{y:.2f}" fill="var(--panel)" {attrs}/>'
    if shape == 'square':
        return f'<rect x="{x-2.5:.2f}" y="{y-2.5:.2f}" width="5" height="5" fill="{color}" {attrs}/>'
    if shape == 'cross':
        return (f'<line x1="{x-2.5:.2f}" y1="{y-2.5:.2f}" x2="{x+2.5:.2f}" y2="{y+2.5:.2f}" {attrs}/>'
                f'<line x1="{x-2.5:.2f}" y1="{y+2.5:.2f}" x2="{x+2.5:.2f}" y2="{y-2.5:.2f}" {attrs}/>')
    return f'<circle cx="{x:.2f}" cy="{y:.2f}" r="2" fill="{color}" {attrs}/>'


def render_chart(spec):
    """Use the single-tank inline SVG approach with native and touch tooltips."""
    identifier = spec['id']
    xmin, xmax = spec['xrange']
    ymin, ymax = spec['yrange']
    categories = spec.get('categories')
    x = lambda value: PLOT_LEFT + (value - xmin) / (xmax - xmin) * (PLOT_RIGHT - PLOT_LEFT)
    y = lambda value: PLOT_BOTTOM - (value - ymin) / (ymax - ymin) * (PLOT_BOTTOM - PLOT_TOP)
    position = lambda point: (PLOT_LEFT + point['row'] * (PLOT_RIGHT - PLOT_LEFT) /
                              (len(categories) - 1)) if categories else x(point['x'])
    parts = []
    if spec.get('validity') is not None:
        parts.append(f'<rect class="band" x="{PLOT_LEFT}" y="{PLOT_TOP}" width="{x(spec["validity"])-PLOT_LEFT:.2f}" height="{PLOT_BOTTOM-PLOT_TOP}" fill="var(--warn)" opacity=".18"/>')
    if categories:
        band = spec['tolerance']
        parts.append(f'<rect class="band" x="{PLOT_LEFT}" y="{y(band):.2f}" width="{PLOT_RIGHT-PLOT_LEFT}" height="{y(-band)-y(band):.2f}" fill="var(--teal)" opacity=".14"/>')
    for axis, low, high, step in (('x', xmin, xmax, spec['xstep']), ('y', ymin, ymax, spec['ystep'])):
        if axis == 'x' and categories:
            for i, label in enumerate(categories):
                px = PLOT_LEFT + i * (PLOT_RIGHT-PLOT_LEFT) / (len(categories)-1)
                # Separate rows keep the five case labels legible on narrow screens.
                parts.append(svg_text(px, PLOT_BOTTOM + 20 + (i % 2) * 32, label,
                                      wrap=9, text_anchor=('start' if i == 0 else 'end' if i == len(categories)-1 else 'middle'), class_='x-tick'))
            continue
        precision = max(1 if (spec['xunit' if axis == 'x' else 'yunit'] == '%') else 0,
                        -math.floor(math.log10(step)))
        start, end = math.ceil(low / step), math.floor(high / step)
        for i in range(start, end + 1):
            value = i * step
            label = f'{value:.{precision}f}'.replace('-', '−')
            if axis == 'x':
                parts.append(svg_text(f'{x(value):.2f}', PLOT_BOTTOM + 20, label,
                                      text_anchor='middle', class_='x-tick', data_value=value))
            else:
                parts.append(f'<line x1="{PLOT_LEFT}" x2="{PLOT_RIGHT}" y1="{y(value):.2f}" y2="{y(value):.2f}" stroke="var(--grid)"/>')
                parts.append(svg_text(PLOT_LEFT-7, f'{y(value)+4:.2f}', label,
                                      text_anchor='end', class_='y-tick', data_value=value))
    xlabel_y = PLOT_BOTTOM + (98 if categories else 48)
    parts.append(svg_text((PLOT_LEFT+PLOT_RIGHT)/2, xlabel_y, spec['xlabel'],
                          text_anchor='middle', class_='x-label', wrap=32))
    parts.append(svg_text(0, 0, spec['ylabel'], transform='translate(18 142) rotate(-90)',
                          text_anchor='middle', class_='y-label', wrap=29))
    legend_y = xlabel_y + 48
    if spec.get('validity') is not None:
        parts.append(svg_text(12, legend_y, 'outside the reference moment-fit band', fill='var(--ink)'))
        legend_y += 36
    if categories:
        parts.append(svg_text(12, legend_y, f'Reference ±{spec["tolerance"]:.1f} % band', fill='var(--ink)'))
        legend_y += 24
    for index, series in enumerate(spec['series']):
        color = SERIES_COLORS[index % len(SERIES_COLORS)]
        shape = series.get('shape', ('circle', 'square', 'diamond', 'cross')[index % 4])
        points = series['points']
        if spec['continuous']:
            vertices = ' '.join(f'{position(p):.2f},{y(p["y"]):.2f}' for p in points)
            parts.append(f'<polyline points="{vertices}" fill="none" stroke="{color}" stroke-width="1.6"/>')
        for point in points:
            row = dict(zip(spec['headers'], spec['rows'][point['row']]))
            case = (categories[point['row']] if categories else
                    f"{spec['xlabel']}: {format(point['x'], '.1f' if spec['xunit'] == '%' else '.6g')} {spec['xunit']}")
            role = row.get('role', 'independent check' if not spec['continuous'] else 'continuous model evaluation')
            color_point = color
            comparison = ''
            if categories:
                inside = abs(point['y']) <= spec['tolerance']
                comparison = 'inside the band' if inside else 'outside the band (low fill)'
                if not inside:
                    color_point = 'var(--red)'
                comparison += (f"; prediction {row['predicted_period_s']} s; "
                               f"CFD-derived period {row['cfd_period_s']} s")
            fill = f"; fill {row['fill_pct']:.1f} %" if 'fill_pct' in row else ''
            # C1 includes both responses at every period, whichever series has focus.
            both = (f"; Bare vessel {row['roll_bare_per_unit_moment']:.6g} dimensionless; "
                    f"With connected tank {row['roll_with_tank_per_unit_moment']:.6g} dimensionless"
                    if identifier == 'coupled-response' else '')
            yvalue = f"{point['y']:.1f}" if spec['yunit'] == '%' else f"{point['y']:.6g}"
            tooltip = (f"{series['name']}; {case}; {spec['ylabel']}: {yvalue} {spec['yunit']}{fill}; "
                       f"{role}; {comparison}{both}; {chart_reference(identifier)}").replace('−', '-')
            tooltip = re.sub(r'(?<=\s)-(?=\d)', '−', tooltip)
            px, py = position(point), y(point['y'])
            parts.append(f'<g data-series="{index}" data-row="{point["row"]}" data-x="{point["x"]}" data-y="{point["y"]}" data-px="{px:.2f}" data-color="{color_point}" tabindex="0" data-tooltip="{html.escape(tooltip, quote=True)}"><title>{html.escape(tooltip)}</title>')
            if identifier == 'natural-period-vs-area' and series['name'] == 'Calibrated model':
                low, high = point['y'] * (1-spec['tolerance']/100), point['y'] * (1+spec['tolerance']/100)
                parts.append(f'<g class="whisker" data-band="{spec["tolerance"]}" data-source="{html.escape(spec["band_source"], quote=True)}" stroke="{color}"><line x1="{px:.2f}" x2="{px:.2f}" y1="{y(low):.2f}" y2="{y(high):.2f}"/><line x1="{px-4:.2f}" x2="{px+4:.2f}" y1="{y(low):.2f}" y2="{y(low):.2f}"/><line x1="{px-4:.2f}" x2="{px+4:.2f}" y1="{y(high):.2f}" y2="{y(high):.2f}"/></g>')
            parts.append(marker(shape, px, py, color_point) + '</g>')
        parts.append(marker(shape, 17, legend_y-4, color))
        label = ('calibration reference (not an independent check)'
                 if series['name'] == 'Calibration reference' else series['name'])
        parts.append(svg_text(30, legend_y, label, wrap=43, fill=color))
        legend_y += 18 * len(textwrap.wrap(label, 43)) + 10
    if categories:
        parts.append(svg_text(12, legend_y, 'outside the band (low fill)', fill='var(--red)'))
        legend_y += 24
    desc = (f"{spec['ylabel']} against {spec['xlabel']}; x range {xmin:g} to {xmax:g}, "
            f"y range {ymin:g} to {ymax:g}; " + '; '.join(s['name'] for s in spec['series']))
    reference = chart_reference(identifier)
    return (f'<figure class="chart">'
            f'<svg id="{identifier}" class="study-chart" viewBox="0 0 360 {legend_y+12}" role="img" aria-labelledby="{identifier}-title {identifier}-desc">'
            f'<title id="{identifier}-title">{html.escape(spec["title"])}</title>'
            f'<desc id="{identifier}-desc">{html.escape(desc)}</desc>' + ''.join(parts) + '</svg>'
            f'<figcaption>{CHART_CAPTIONS[identifier]}</figcaption>'
            f'<p class="chart-data"><a href="data/charts/{identifier}.csv">Data (CSV)</a> · '
            f'<a href="#ref-chart-{identifier}">{reference}</a></p></figure>')


def schematic():
    """Use the single-tank inline SVG and house colours, with one physical scale.

    Dimensions come from the illustrative inputs used by compute(). Unspecified
    freeboard and the conduit cross-section shape are diagrammatic, not inputs.
    Pixel coordinates below control presentation only.
    """
    scale = 8
    width = ILLUSTRATIVE_LEG_WIDTH * scale
    depth = ILLUSTRATIVE_FILL_DEPTH * scale
    offset = ILLUSTRATIVE_CENTROID_OFFSET * scale
    left, right, base = 180-offset, 180+offset, 280
    mean_level = base-depth
    # Opposite schematic displacements show the level-difference convention.
    # Their size is diagrammatic, with no calculated motion implied.
    displacement = 12
    effective = MODEL.UTubeGeometry(
        leg_length=ILLUSTRATIVE_LEG_LENGTH, leg_width=ILLUSTRATIVE_LEG_WIDTH,
        fill_depth=ILLUSTRATIVE_FILL_DEPTH, centroid_offset=ILLUSTRATIVE_CENTROID_OFFSET,
    ).effective_conduit_length
    labels = [
        f'Each leg width {ILLUSTRATIVE_LEG_WIDTH:.1f} m; centroids ±{ILLUSTRATIVE_CENTROID_OFFSET:.1f} m from centreline',
        f'Fill depth {ILLUSTRATIVE_FILL_DEPTH:.1f} m at mean level; displaced free surfaces: sketch, not a computed state',
        f'conduit, area A_c = {ILLUSTRATIVE_CONDUIT_AREA:.1f} m², effective length {effective:.1f} m (calibrated reference, transfer assumed)',
        f'Leg length {ILLUSTRATIVE_LEG_LENGTH:.1f} m into the page; tank height not specified',
        'archived CFD connected-tank geometry is not shown',
    ]
    parts = ['<svg id="schematic" viewBox="0 0 360 420" role="img" aria-labelledby="schematic-title schematic-desc">',
             '<title id="schematic-title">Illustrative connected-tank system, transverse section</title>',
             '<desc id="schematic-desc">' + html.escape('; '.join(labels)) + '</desc>',
             '<defs><marker id="dimension-arrow" markerWidth="6" markerHeight="6" refX="3" refY="3" orient="auto-start-reverse"><path d="M 6 0 L 0 3 L 6 6" fill="none" stroke="var(--mut)"/></marker></defs>']

    def dimension(x1, y1, x2, y2, label, tx, ty, identifier=''):
        id_attribute = f' id="{identifier}"' if identifier else ''
        return (f'<g class="dimension"{id_attribute} stroke="var(--mut)" fill="none">'
                f'<line x1="{x1}" y1="{y1}" x2="{x2}" y2="{y2}" marker-start="url(#dimension-arrow)" marker-end="url(#dimension-arrow)"/>'
                f'<text x="{tx}" y="{ty}" stroke="none" fill="var(--ink)" text-anchor="middle">{html.escape(label)}</text></g>')

    for center, name, level, leader in (
            (left, 'Left leg', mean_level-displacement, left-27),
            (right, 'Right leg', mean_level+displacement, right+18)):
        x = center-width/2
        parts.append(f'<rect class="tank-leg" x="{x}" y="172" width="{width}" height="{base-172}" fill="var(--panel)"/>')
        parts.append(f'<rect class="water-depth" x="{x}" y="{level}" width="{width}" height="{base-level}" fill="var(--teal)" fill-opacity=".15"/>')
        parts.append(f'<path d="M {x} 172 V {base} H {x+width} V 172" fill="none" stroke="var(--navy)" stroke-width="1.5"/>')
        parts.append(f'<line class="free-surface" x1="{x}" x2="{x+width}" y1="{level}" y2="{level}" stroke="var(--teal)" stroke-width="1.5"/>')
        parts.append(svg_text(center, 108, name, text_anchor='middle'))
        parts.append(svg_text(center, 145, 'free surface', text_anchor='middle'))
        parts.append(f'<line x1="{leader}" x2="{leader}" y1="158" y2="{level}" stroke="var(--teal)"/>')
        centroid_start = 200 if center == left else 172
        parts.append(f'<line x1="{center}" x2="{center}" y1="{centroid_start}" y2="{base+10}" stroke="var(--mut)" stroke-dasharray="4 4"/>')
    arc_end_x = 180 + 28*math.sin(math.radians(30))
    arc_end_y = 365 - 28*math.cos(math.radians(30))
    parts.extend([
        f'<rect x="{left+width/2}" y="{base-14}" width="{right-left-width}" height="14" fill="var(--teal)" fill-opacity=".15" stroke="var(--navy)" stroke-width="1.5"/>',
        dimension(left-width/2, 195, left+width/2, 195, f'{ILLUSTRATIVE_LEG_WIDTH:.1f} m', left, 181),
        dimension(left, 36, right, 36, f'{2*ILLUSTRATIVE_CENTROID_OFFSET:.1f} m between centroids', 180, 22),
        f'<path d="M {right+width/2} {mean_level} H 330 M {right+width/2} {base} H 330" fill="none" stroke="var(--mut)"/>',
        dimension(330, mean_level, 330, base, f'{ILLUSTRATIVE_FILL_DEPTH:.1f} m', 320, 316),
        f'<line x1="{right+18}" x2="{right+width/2}" y1="{mean_level}" y2="{mean_level}" stroke="var(--mut)" stroke-dasharray="4 4"/>',
        '<line x1="180" x2="180" y1="62" y2="280" stroke="var(--mut)" stroke-dasharray="4 4"/>',
        f'<line class="level-extension" x1="{left+width/2}" x2="166" y1="{mean_level-displacement}" y2="{mean_level-displacement}" stroke="var(--teal)" stroke-dasharray="4 4"/>',
        f'<line class="level-extension" x1="166" x2="{right-width/2}" y1="{mean_level+displacement}" y2="{mean_level+displacement}" stroke="var(--teal)" stroke-dasharray="4 4"/>',
        dimension(166, mean_level-displacement, 166, mean_level+displacement, 'Δh', 200, mean_level+3, 'level-difference'),
        '<line x1="180" x2="180" y1="280" y2="290" stroke="var(--mut)"/>',
        svg_text(180, 311, f'conduit A_c = {ILLUSTRATIVE_CONDUIT_AREA:.1f} m²', text_anchor='middle'),
        '<line id="roll-reference" x1="180" x2="180" y1="327" y2="365" stroke="var(--mut)" stroke-dasharray="4 4"/>',
        f'<line id="roll-ray" x1="180" y1="365" x2="{180+38*math.sin(math.radians(30))}" y2="{365-38*math.cos(math.radians(30))}" stroke="var(--navy)"/>',
        f'<path id="roll-angle" d="M 180 337 A 28 28 0 0 1 {arc_end_x} {arc_end_y}" fill="none" stroke="var(--navy)" stroke-width="1.5"/>',
        '<line x1="180" x2="180" y1="365" y2="386" stroke="var(--mut)"/>',
        svg_text(180, 410, 'roll angle φ', text_anchor='middle'),
    ])
    parts.append('</svg>')
    return ('<figure class="schematic">' + ''.join(parts) +
            '<figcaption>Illustrative connected-tank transverse section.</figcaption>'
            f'<p class="figure-note">Leg length {ILLUSTRATIVE_LEG_LENGTH:.1f} m into the page; centroids ±{ILLUSTRATIVE_CENTROID_OFFSET:.1f} m. '
            f'Effective conduit length {effective:.1f} m (calibrated reference; transfer assumed). '
            'Tank height is not specified; archived CFD geometry is not shown. '
            'Widths, centroid spacing and fill depth share one scale; unspecified freeboard and conduit shape are diagrammatic. '
            'All dimensions are assumed inputs in <a href="#geometry">Section 3</a>; Δh and roll angle indicate the motion convention, not a computed state.</p></figure>')


LAYOUT_CSS = """
figure{margin:18px 0;min-width:0}svg{width:100%;height:auto;display:block;color:var(--ink);font:inherit;font-size:13px;fill:var(--ink)}
figcaption{font-size:.82rem;color:var(--navy)}.sources{font-size:.8rem;padding-left:20px}.sources code{overflow-wrap:anywhere}
.chart{border:1px solid var(--grid);border-radius:6px;padding:8px;background:var(--panel)}
.chart g[tabindex]:focus{outline:2px solid var(--accent)}nav{overflow-wrap:anywhere}
table{table-layout:fixed}td,th{overflow-wrap:anywhere}#chart-tooltip{position:fixed;bottom:10px;left:10px;max-width:calc(100vw - 20px);width:320px;padding:12px;background:var(--panel);color:var(--ink);border:1px solid var(--navy);z-index:2;overflow-wrap:anywhere;font-size:.85rem}
@media(max-width:600px){.wrap{padding-left:8px;padding-right:8px}.chart{padding:4px}th,td{padding:5px 3px;font-size:.75rem}}
.study-chart{max-width:440px;margin:0 auto}.study-chart text{font-size:13px;font-weight:400}
.schematic svg{max-width:440px;font-size:16px;margin:0 auto}.figure-note,.chart-data{font-size:.82rem}
caption{caption-side:bottom;text-align:left;font-size:.82rem;color:var(--mut);padding:6px 0}
pre{white-space:pre-wrap;overflow-wrap:anywhere;font-size:.85rem}
code{overflow-wrap:anywhere}
@media print{
  @page{margin:16mm}
  body,.wrap{background:#fff;box-shadow:none}.wrap{max-width:none;padding:0}
  nav,.chart-data,#chart-tooltip{display:none!important}
  figure,table{break-inside:avoid}h2,h3{break-after:avoid}
  .study-chart{max-width:340px}.study-chart text{font-size:13px;font-weight:400}
  .schematic svg{max-width:340px;font-size:16px}
  figcaption,caption,.figure-note{font-size:9pt;font-weight:400}
  td,th{font-size:9pt}thead{display:table-header-group}
  a{color:inherit;text-decoration:none}
}

"""

TOOLTIP_SCRIPT = """<script>
(() => {
  const box = document.createElement('div');
  box.id = 'chart-tooltip'; box.setAttribute('role', 'status'); box.hidden = true;
  document.body.append(box);
  const show = event => {
    const point = event.target.closest('[data-tooltip]') || document.activeElement.closest('[data-tooltip]');
    box.hidden = !point;
    if (point) box.textContent = point.dataset.tooltip;
  };
  for (const name of ['pointerover', 'focusin', 'pointerdown']) document.addEventListener(name, show);
  document.addEventListener('keydown', event => { if (event.key === 'Escape') box.hidden = true; });
  document.addEventListener('focusout', () => { box.hidden = true; });
  document.addEventListener('pointerout', event => {
    if (event.pointerType !== 'touch') {
      const focused = document.activeElement.closest('[data-tooltip]');
      if (focused) show({target: focused});
      else box.hidden = true;
    }
  });
})();
</script>"""


def format_percentage_text(value):
    """Round percentage prose, preserving other measurements and integer counts."""
    def rounded(match):
        precision = 1
        if match[3] is not None and float(match[1]) != float(match[3]):
            if f'{float(match[1]):.1f}' == f'{float(match[3]):.1f}':
                precision = 2
        display = f"{float(match[1]):.{precision}f}"
        if match[3] is not None:
            separator = '–' if precision == 2 else match[2]
            display += f"{separator}{float(match[3]):.{precision}f}"
        return display + " %"

    return re.sub(
        r"(?<![\w.])([+-]?\d+(?:\.\d+)?)(?:\s*([-–])\s*([+-]?\d+(?:\.\d+)?))?\s*%",
        rounded, value,
    )


def evidence_value(fact, precision=1, scale=1):
    """Bind a visible scalar to its raw evidence value and source pointer."""
    value = fact["value"]
    display = (f"{value * scale:.{precision}f}" if isinstance(value, (int, float))
               else format_percentage_text(str(value)).replace('twin-tank', 'connected-tank'))
    source = fact["provenance"]
    locator = source["path"] + "#" + source["pointer"]
    return (f'<data value="{html.escape(str(value), quote=True)}" '
            f'data-provenance="{html.escape(locator, quote=True)}">'
            f'{html.escape(display)}</data>')


def evidence_table(headers, rows, caption):
    head = "".join(f"<th>{html.escape(label)}</th>" for label in headers)
    body = "".join("<tr>" + "".join(f"<td>{cell}</td>" for cell in row) + "</tr>" for row in rows)
    return f"<table><tr>{head}</tr>{body}</table><p class=\"sub\">{caption}</p>"



def area_period_pairs(data):
    """Join independent reductions by physical area, never by row position."""
    calibration = data["calibration"]
    checks = {r["area"]["value"]: r for r in data["period_checks"]
              if r["fill"]["value"] == calibration["fill"]["value"]}
    pairs = []
    for row in data["area_sweep"]:
        area = row["area"]["value"]
        period = calibration if area == calibration["area"]["value"] else checks[area]
        pairs.append((row, period["measured_period"], period["predicted_period"]))
    return pairs


def render_cfd_evidence():
    """Render the archive projection using only the committed evidence JSON."""
    d = json.loads(EVIDENCE_PATH.read_text(encoding="utf-8"))
    v = evidence_value
    def val(fact):
        return fact["value"]
    charts = {spec["id"]: render_chart(spec) for spec in chart_specs(evidence=d, evidence_only=True)}
    table = evidence_table
    rows = d["area_sweep"]
    conditions = d["area_conditions"]
    checks = d["period_checks"]
    calibration = d["calibration"]
    areas = [str(val(r["area"])) for r in rows]
    part = ['<section id="cfd-evidence">',
            '<h2 id="archive">Appendix D – Archived CFD comparisons</h2>',
            '<p>These comparisons use the separate archived connected-tank campaign introduced in Section 2. '
            'Its geometry is not shown. '
            'The <a href="data/dual_tank_cfd_evidence.json">evidence record</a> identifies each reduction. '
            'No fitted curve: the archived geometry is not published, so the model cannot be drawn between the cases from committed inputs.</p>',
            charts['natural-period-vs-area'],
            '<p>The calibration case is omitted from the period-error view because it is not an independent check.</p>',
            charts['period-error'],
            '<h3 id="cfd-area">D.1 Conduit-area sweep</h3>',
            f'<p>Exchange amplitudes are compared at forcing period {v(conditions["period"], 2)} s '
            f'and imposed roll {v(conditions["roll"])}°. This is the imposed forcing condition of the archived area sweep, '
            'not a measured exchange natural period. The record does not establish why that forcing period was selected. '
            'Exchange amplitude is the volume displaced between the legs about its mean (m³). '
            'Ratios are normalized to the smallest area. '
            'The historical analytical amplitude uses geometric conduit length; it is not the later calibrated model.</p>']
    part.append(charts["cfd-amplitude"])
    part.append(table(["Area (m²)", "CFD amplitude (m³)", "Analytical amplitude (m³)", "CFD ratio", "Analytical ratio"],
                      [[v(r["area"]), v(r["cfd_amplitude"]), v(r["analytical_amplitude"]),
                        v(r["cfd_ratio"], 3), v(r["analytical_ratio"], 3)] for r in rows],
                      "Exchange-amplitude comparison; ratios retain the archived precision."))
    pairs = area_period_pairs(d)
    measured = [m for _, m, _ in pairs]
    predicted = [p for _, _, p in pairs]
    part.append('<p>The area-period comparison is plotted at the start of this subsection.</p>')
    part.append(table(["Area (m²)", "CFD-derived natural period (s)", "Calibrated prediction (s)", "Historical geometric-length prediction (s)"],
                      [[v(r["area"]), v(m), v(p), v(r["historical_period"])] for r, m, p in pairs],
                      "Period bases differ: the historical period is analytical, not a CFD measurement. The middle area is the calibration reference."))
    band = d["published_band"]
    part += ['<h3 id="cfd-inertia">D.2 Inertia and natural period</h3>',
             f'<p>The archive records a ±{v(band, 0)} % band for the inertia law using one calibrated effective conduit length. '
             'The reductions below include an exception: the low-fill check exceeds that band. '
             'The error is prediction minus CFD-derived period, divided by CFD-derived period. '
             'This checks exchange-mode inertia through natural period; it does not validate frozen-fluid roll inertia. '
             f'The calibration period is {v(calibration["predicted_period"], 2)} s against '
             f'{v(calibration["measured_period"], 2)} s from the reduction; it is not an independent check.</p>']
    labels = [f'{val(r["area"]):.1f} / {val(r["fill"]):.0f}' for r in checks]

    part.append(table(["Area (m²)", "Fill (%)", "Prediction (s)", "CFD-derived (s)", "Error (%)", "Band comparison"],
                      [[v(r["area"]), v(r["fill"], 0), v(r["predicted_period"]), v(r["measured_period"]),
                        v(r["error"]), "Outside: exception" if abs(val(r["error"])) > val(band) else "Inside"] for r in checks],
                      "All archived off-calibration period checks; the independent intermediate-area check is included."))
    fill = sorted([r for r in checks if val(r["fill"]) != val(calibration["fill"])
                   and val(r["area"]) == val(calibration["area"])], key=lambda r: val(r["fill"]))
    part += ['<h3 id="cfd-fill">D.3 Fill sweep</h3>',
             '<p>Discrete low and high fill checks bracket the calibration fill; continuous fill coverage is not established. '
             'Equivalent damping depends on forcing period and imposed roll. The table retains each available condition; '
             'the damping chart selects the lower-roll case nearest each predicted natural period.</p>']
    chosen = [min(r["damping"], key=lambda c: (val(c["roll"]), abs(val(c["period"]) - val(r["predicted_period"])))) for r in fill]
    fill_labels = [f'{val(r["fill"]):.0f}' for r in fill]
    part.append(charts["cfd-fill-period"])
    part.append(charts["cfd-fill-damping"])
    part.append(table(["Fill (%)", "Area (m²)", "Forcing period (s)", "Roll (°)", "Equivalent damping ratio (dimensionless)"],
                      [[v(r["fill"], 0), v(r["area"]), v(c["period"]), v(c["roll"]), v(c["measured"], 3)] for r in fill for c in r["damping"]],
                      "Fill-dependent damping at the archived forcing conditions; period predictions and measurements appear above."))
    loss = d["loss"]
    third = loss["third"]
    part += ['<h3 id="cfd-loss">D.4 Loss scaling and independent area check</h3>',
             f'<p>The fitted area exponent is n = {v(loss["exponent"], 3)}, with {v(loss["rms"])} % RMS '
             f'over {v(loss["cases"], 0)} cases. The naive inverse-area form has '
             f'{v(loss["linear_rms"])} % RMS after its own coefficient refit: it underpredicts damping at the smallest area '
             f'(group RMS {v(loss["small_area_rms"])} %) and overpredicts at the largest '
             f'(group RMS {v(loss["large_area_rms"])} %). These group RMS magnitudes are distinct from signed individual errors.</p>']
    # Use matched near-natural-period conditions, not an unqualified average of the loss-fit surface.
    loss_checks = sorted([r for r in checks if val(r["fill"]) == val(calibration["fill"])], key=lambda r: val(r["area"]))
    near = [min(r["damping"], key=lambda c: abs(val(c["period"]) - val(r["predicted_period"]))) for r in loss_checks]
    part.append(charts["cfd-loss-area"])
    part.append(table(["Area (m²)", "Fill (%)", "Forcing period (s)", "Roll (°)", "CFD-derived damping (dimensionless)", "Original inverse-area damping (dimensionless)"],
                      [[v(r["area"]), v(r["fill"], 0), v(c["period"]), v(c["roll"]), v(c["measured"], 3), v(c["linear_prediction"], 3)] for r, c in zip(loss_checks, near)],
                      "Conditions near each predicted natural period. The original inverse-area prediction uses the reference calibration, not the separate all-data coefficient refit."))
    part += [f'<p>At the independent intermediate area, the existing law evaluated without refitting gives '
             f'{v(third["independent_rms"])} % RMS. Including that area in the fit gives '
             f'n = {v(third["exponent"], 3)} and {v(third["rms"])} % RMS over '
             f'{v(third["cases"], 0)} cases. Percentages are rounded to one decimal place; '
             'the evidence record retains the source precision. '
             'The independent prediction and all-data refit are separate checks.</p>',
             '<h3 id="cfd-forced">D.5 Forced-roll period sweep and numerical checks</h3>',
             '<p>The medium-mesh forced-roll cases are accepted for screening after cycle convergence. '
             'The exchange plateau is the broad region of similar forced exchange amplitudes across adjacent forcing periods; '
             'it is separate from the exchange natural period. Fine-mesh confirmation applies at the selected period only; '
             'it does not establish convergence throughout the sweep.</p>']
    sweep = d["forced_sweep"]
    part.append(charts["cfd-forced-exchange"])
    part.append(table(["Forcing period (s)", "Exchange amplitude (m³)", "Roll-moment amplitude (MN m)"],
                      [[v(r["period"], 0), v(r["exchange"]), v(r["moment"], scale=1e-6)] for r in sweep],
                      "Completed, cycle-converged medium-mesh sweep."))
    mesh, frequency = d["mesh"], d["frequency"]
    part.append(table(["Numerical check", "Result"], [
        ["Fine confirmation period (s)", v(mesh["period"], 0)],
        ["Exchange change (%)", v(mesh["exchange_error"])],
        ["Moment change (%)", v(mesh["moment_error"])],
        ["Fine transient maximum Courant / nominal target", v(mesh["max_courant"], 3) + " / " + v(mesh["courant_target"])],
        ["Free-decay frequency error (%)", v(frequency["minimum_error"], 2) + "–" + v(frequency["maximum_error"], 2)],
        ["Observed Richardson order", v(frequency["richardson_order"], 3)],
        ["Indicative fine-grid GCI (%)", v(frequency["gci"])],
    ], "Fine-mesh Courant exceeds the nominal target. Free-decay convergence is non-asymptotic; the GCI is indicative. Free-decay validates frequency in its own case family, not connected-tank anti-roll response."))
    part.append('<p>Analytical screening cases are not CFD solves. Incomplete case attempts and the open-boundary transfer check are excluded from the accepted evidence. '
                'Short transfer checks are verification only. Trustworthy phase and anti-roll benefit remain withheld. '
                'The evidence classification is in <a href="#cfd-classes">Appendix A</a>.</p>')
    return "\n".join(part) + "\n</section>"


CHART_ORDER = (
    'coupled-response', 'natural-period-vs-area', 'period-error', 'natural',
    'inertia', 'damping', 'moment', 'split', 'cfd-amplitude', 'cfd-fill-period',
    'cfd-fill-damping', 'cfd-loss-area', 'cfd-forced-exchange',
)

# Short captions complement the full accessible chart titles and lead-in text.
CHART_CAPTIONS = dict(zip(CHART_ORDER, (
    'Illustrative coupled roll response.', 'Archived period against conduit area.',
    'Off-calibration period error.', 'Illustrative natural period.',
    'Illustrative frozen-fluid inertia.', 'Illustrative quadratic-loss damping.',
    'Illustrative tank moment.', 'Single-tank response comparison.',
    'Archived exchange amplitude.', 'Archived period against fill.',
    'Archived damping against fill.', 'Archived damping against area.',
    'Archived forced exchange.',
)))


def chart_reference(identifier):
    """Reserve the first seven references for the model and evidence records."""
    return f'[I{CHART_ORDER.index(identifier) + 8}]'


def software_version():
    """Read the declared analysis package version rather than inventing one."""
    return tomllib.loads((ROOT / 'pyproject.toml').read_text())['project']['version']


def reference_sections(specs):
    """Give the public scope reference and a short data-availability statement."""
    return ('<h3>9.1 Public references</h3>'
            '<p>No external publication is used to qualify these connected-tank results. '
            'The public single-tank study is a presentation and analytical-scope reference only: '
            '<a href="sloshing-cfd-study.html">single-tank sloshing study</a>. '
            'Related work: <a href="generic-hull-roll-screening.html">hull-roll screening analysis</a>.</p>'
            '<h3>9.2 Data availability</h3>'
            '<p>The reduced comparison data and analytical response record are linked in '
            '<a href="#reproduce">Appendix B</a>; archived study files are available on request.</p>')


def source_appendix():
    """Keep citation targets while omitting unresolvable archive paths."""
    sources = [('model', '[I1] Analytical connected-tank model and calibration basis.'),
               ('tests', '[I2] Reference values from the study’s test inputs.'),
               ('evidence', '[I3] <a href="data/dual_tank_cfd_evidence.json">Archived CFD reduction record</a>.'),
               ('reduction', '[I4] Single-tank tuned-absorber method.'),
               ('response', '[I5] <a href="../structural/sloshing-tld-twopeak.json">Single-tank response record</a>.'),
               ('software', f'[I6] Analysis software: digitalmodel {software_version()}; archived CFD solver version is not identified.'),
               ('build', '[I7] Analytical page and CSV generation.')]
    markers = ''.join(f'<span id="ref-chart-{identifier}">{chart_reference(identifier)} </span>'
                      for identifier in CHART_ORDER)
    return ('<ul class="refs">' + ''.join(f'<li id="ref-{identifier}">{description}</li>'
                                        for identifier, description in sources) + '</ul>'
            '<p>' + markers + 'archived study files, available on request.</p>')


def localize_references(page):
    """Replace body source links with resolvable reference markers."""
    before, references = page.split('<h2 id="basis">', 1)
    targets = {
        '../../../src/digitalmodel/hydrodynamics/utube_tank.py': ('model', '[I1]'),
        '../../../tests/hydrodynamics/test_utube_tank.py': ('tests', '[I2]'),
        'data/dual_tank_cfd_evidence.json': ('evidence', '[I3]'),
        '../../../scripts/cfd/reduce_tld_two_peak.py': ('reduction', '[I4]'),
        '../structural/sloshing-tld-twopeak.json': ('response', '[I5]'),
    }
    for path, (identifier, marker) in targets.items():
        before = re.sub(r'<a href="' + re.escape(path) + r'">(.*?)</a>',
                        lambda match: match[1] + f' <a href="#ref-{identifier}">{marker}</a>',
                        before, flags=re.S)
    before = before.replace('natural period [I1]', 'natural period <a href="#ref-model">[I1]</a>')
    before = before.replace('Reference inputs [I2]', 'Reference inputs <a href="#ref-tests">[I2]</a>')
    return before + '<h2 id="basis">' + references


def number_objects(page):
    """Number figures and short table captions by their containing section."""
    table_titles = {
        '3': ['Illustrative geometry and assumptions'],
        '5': ['Single-tank response peaks'],
        'D': ['Exchange amplitudes', 'Natural periods by area',
              'Off-calibration period checks', 'Fill-dependent damping',
              'Near-natural-period damping', 'Forced-roll sweep', 'Numerical checks'],
        '6': ['Reference calibration quantities'],
        'A': ['Sensitivity and evidence matrix', 'Evidence classes'],
        'C': ['Revision history'],
    }
    chunks = re.split(r'(<h2 id="[^"]+">.*?</h2>)', page)
    section = '0'
    for index, chunk in enumerate(chunks):
        if chunk.startswith('<h2'):
            label = re.sub('<[^>]+>', '', chunk)
            section = ('R' if label == 'Revision history' else
                       label.split()[1] if label.startswith('Appendix') else label.split()[0])
            continue
        figure_count = table_count = 0

        def figure(match):
            nonlocal figure_count
            figure_count += 1
            return f'<figcaption>Figure {section}.{figure_count} – {match[1]}</figcaption>'

        def table(match):
            nonlocal table_count
            table_count += 1
            title = table_titles[section][table_count-1]
            return f'<table><caption>Table {section}.{table_count} – {title}</caption>'

        chunk = re.sub(r'<figcaption>(.*?)</figcaption>', figure, chunk, flags=re.S)
        chunks[index] = re.sub(r'<table>', table, chunk)
    return ''.join(chunks)


def sensitivity_matrix(data):
    """Describe the existing case coverage without adding calculated sensitivities."""
    p = data['inputs']
    return '<div id="sensitivity-matrix">' + evidence_table(['Input or evidence', 'Existing coverage', 'Affected result and remaining limit'], [
        ['Leg dimensions and centroid offset', 'ASSUMED illustrative configuration', 'Period, inertia and roll; geometry transfer is unqualified [I1]'],
        ['Conduit area (m²)', f'{min(data["areas"]):.1f}–{max(data["areas"]):.1f}', 'Continuous illustrative curves; discrete archived comparisons [I2, I3]'],
        ['Forcing period (s)', f'{min(data["periods"]):.1f}–{max(data["periods"]):.1f}', 'Moment and roll; applicability band retained [I1]'],
        ['Level difference (m)', ' / '.join(f'{level:.1f}' for level in p['levels']), 'Amplitude-dependent damping; no fixed tank damping [I2]'],
        ['Effective conduit length', 'ASSUMED calibration transfer', 'New natural-period measurement required [I1]'],
        ['Fill and loss', 'Discrete archived conditions only', 'No continuous fill coverage; independent prediction and all-data refit remain distinct [I3]'],
        ['Single-tank comparison', 'Mass, tuning, hull damping and forcing unmatched', 'Peak splitting and merging only [I4, I5]'],
        ['Numerical confirmation', 'Selected period only', 'Courant exceeds nominal target; non-asymptotic free-decay convergence; indicative GCI [I3]'],
        ['Operating conditions', 'Freeboard, loads and dimensional coupling not assessed', 'Design qualification and trustworthy anti-roll phase and benefit remain unestablished [I3]'],
    ], 'Existing sweeps and evidence gaps; no additional sensitivity bounds are inferred.') + '</div>'


def evidence_classes():
    data = json.loads(EVIDENCE_PATH.read_text())
    return '<div id="cfd-classes">' + evidence_table(
        ['Evidence class', 'Result family', 'Disposition', 'Finding', 'Limitation'],
        [[evidence_value(row[key]) for key in ('evidence_class', 'result_family', 'status', 'key_finding', 'limitation')]
         for row in data['evidence_classes'] if row['result_family']['value'] != 'nine selected cases'],
        'The archived evidence classification distinguishes completed comparisons, rejected attempts and methodology-only calculations.',
    ) + '</div>'


def render():
    d = compute()
    c = published_claims()
    p, geometry = d["inputs"], d["geometry"]
    style = SHOWCASE._CSS + LAYOUT_CSS
    claims = (
        ("Quadratic conduit loss: calibrated area exponent", c["area_exponent"]),
        ('Calibration cases (aggregate results only)', c["cases"]),
        ("Calibrated loss-law RMS (%)", c["loss_rms"]),
        ("Refitted inverse-area law RMS (%)", c["linear_rms"]),
        ("Closed-form moment RMS (%)", c["moment_rms"]),
        ("Moment fit applies from forcing period (s)", c["moment_min_period"]),
        ("Degradation reported below about (s)", c["degrades_below"]),
        ("Forced-sweep / natural-period calibrated effective length (times)", c["length_inflation"]),
    )
    claim_rows = "".join(f"<tr><td>{label}</td><td>{value}</td></tr>" for label, value in claims)
    absorber_rows = "".join(
        f'<tr><td>{a["zeta"]*100:.1f}</td><td>{a["peaks"]}</td>'
        f'<td>{max(a["ys"]):.1f}</td></tr>' for a in d["absorbers"]
    )
    specs = {spec['id']: spec for spec in chart_specs(d)}
    charts = {identifier: render_chart(spec) for identifier, spec in specs.items()}
    svg_period, svg_inertia, svg_damping, svg_moment, svg_roll, svg_split = (
        charts[key] for key in ('natural', 'inertia', 'damping', 'moment', 'coupled-response', 'split'))
    evidence = json.loads(EVIDENCE_PATH.read_text(encoding='utf-8'))
    v = evidence_value
    loss, calibration = evidence['loss'], evidence['calibration']
    checks, band = evidence['period_checks'], evidence['published_band']
    inside = sum(abs(row['error']['value']) <= band['value'] for row in checks)
    words = ('zero', 'one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine', 'ten')
    low_fill = next(row for row in checks if abs(row['error']['value']) > band['value'])
    count_text = (f'<data value="{inside}" data-provenance="data/dual_tank_cfd_evidence.json#/period_checks">{words[inside]}</data> of '
                  f'<data value="{len(checks)}" data-provenance="data/dual_tank_cfd_evidence.json#/period_checks">{words[len(checks)]}</data>')
    # Peaks use precisely the rows written to the response CSV.
    response_rows = specs['coupled-response']['rows']
    bare_peak = max(row[1] for row in response_rows)
    tank_peak = max(row[2] for row in response_rows)
    page = f'''<!DOCTYPE html>
<html lang="en" data-theme="light"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Connected-tank sloshing analysis</title>
<link rel="stylesheet" href="../_assets/brand.css"><style>{style}</style></head>
<body><div class="wrap"><header><div class="kicker">Hydrodynamics · analytical study</div>
<h1>Connected-tank sloshing analysis</h1>
<p class="sub">Connected-tank anti-roll arrangement: analytical screening model calibrated on archived CFD</p>
<div class="meta"><div><span>Analytical model</span></div><div><span>{v(loss["cases"], 0)} archived CFD cases (aggregate)</span></div><div><span>Period within ±{v(band, 0)} % at {inside} of {len(checks)} checks</span></div></div></header>
<nav aria-label="Contents"><a href="#summary">1 Executive summary</a> ·
<a href="#introduction">2 Introduction</a> · <a href="#geometry">3 Design data</a> ·
<a href="#method">4 Analysis methodology</a> · <a href="#curves">5 Results</a> ·
<a href="#validation">6 Validation and benchmark</a> · <a href="#limits">7 Limitations</a> ·
<a href="#forward">8 Conclusions and next steps</a> · <a href="#basis">9 References</a> ·
<a href="#sensitivity">A Sensitivity matrix</a> · <a href="#reproduce">B Data and source notes</a> ·
<a href="#revisions">C Revision history</a> · <a href="#archive">D Archived comparisons</a></nav>
<h2 id="summary">1 Executive summary</h2>
<p>Use these results to examine connected-tank response and select the measurements needed before design application.</p>
<p>These results support screening only; design qualification is not established. Transfer of calibration to the invented geometry is assumed; continuous fill coverage and anti-roll phase remain unestablished. The anti-roll benefit on a real vessel is not established. See <a href="#limits">Section 7</a>.</p>
<ul><li>Conduit loss: a calibrated area exponent of {v(loss["exponent"], 3)} fits the archived damping with {v(loss["rms"])} % RMS over {v(loss["cases"], 0)} cases. The simple inverse-area law, refitted on its own, gives {v(loss["linear_rms"])} % RMS.</li>
<li>Natural period: the calibrated model reproduces the CFD-derived exchange natural period within the reference ±{v(band, 0)} % band at {count_text} off-calibration checks. The low-fill check ({v(low_fill["fill"], 0)} %) is {v(low_fill["error"])} % high. The calibration case agrees by construction ({v(calibration["predicted_period"], 2)} s against {v(calibration["measured_period"], 2)} s).</li>
<li>Response: in the illustrative screening prediction, with the tank, the predicted peak roll of the invented system falls from {bare_peak:.1f} to {tank_peak:.1f} per unit wave moment. The anti-roll benefit on a real vessel is not established.</li></ul>
<p>Alpha is the normalized stiffness scale: α = K_tank / K_hull, where K_tank is the quasi-static tank moment per radian and K_hull is the hull restoring stiffness (normalized to one in this model); it is distinct from tank-to-vessel mass ratio. The illustrative response uses alpha = {p["authority"]*100:.1f} %, an ASSUMED input <a href="#ref-model">[I1]</a> <a href="#ref-tests">[I2]</a>.</p>

<h2 id="introduction">2 Introduction</h2>

<p>This study extends the analytical scope of the single-tank sloshing study to assess a U-tube style anti-roll arrangement: two connected tanks with invented geometry, joined by a conduit. The geometry represents no particular vessel or client. The connected-tank model is an analytical screening model calibrated on CFD assessment: the aggregate results of 35 cases from an earlier, archived CFD campaign on a separate connected-tank geometry. Analysis is performed to determine the connected-tank response and how closely the calibrated model reproduces the archived CFD; the key results are summarized in <a href="#summary">Section 1</a>.</p>
<p>The audience is engineers assessing the analytical response and its evidence limits. Raw CFD fields, meshes and the archived geometry are outside this report; the illustrative configuration is distinct from the archived configuration.</p>
<h2 id="geometry">3 Design data</h2>
<p>The illustrative inputs define the calculated configuration; archived comparison conditions remain separate. Assumptions are marked below and their effects are listed in <a href="#sensitivity">Appendix A</a>.</p>
<h3>3.1 Site or field data</h3><p>Not applicable: the configuration is invented and has no site-specific basis.</p>
<h3>3.2 Structure or equipment data</h3>

<p>Invented round dimensions are selected independently of the
<a href="../../../tests/hydrodynamics/test_utube_tank.py">reference geometry from the study’s test inputs</a>.
Validation of the illustrative plan areas and centroid offset is not established. Fill depth is an
input; a validated fill-percentage range is not established.</p>
<table><tr><th>Input</th><th>Illustrative value</th><th>Source and assumption</th><th>Basis</th></tr>
<tr><td>Each leg length × width</td><td>{geometry.leg_length:.1f} × {geometry.leg_width:.1f} m</td><td>Independent invented round dimensions</td><td class="assumption">ASSUMED – to be confirmed by measured geometry and natural period [I1]</td></tr>
<tr><td>Each leg plan area</td><td>{geometry.leg_area:.1f} m²</td><td>Product of the illustrative leg length and width</td><td class="assumption">Derived from ASSUMED dimensions</td></tr>
<tr><td>Leg centroid offset from centreline</td><td>{geometry.centroid_offset:.1f} m</td><td>Independent invented round offset</td><td class="assumption">ASSUMED – to be confirmed by measured geometry and natural period [I1]</td></tr>
<tr><td>Fill depth</td><td>{geometry.fill_depth:.1f} m</td><td>Independent invented round depth</td><td class="assumption">ASSUMED – to be confirmed by measured geometry and natural period [I1]</td></tr>
<tr><td>Conduit area for period sweeps</td><td>{d["area"]:.1f} m²</td><td>Independent invented area within the reference conduit-area input range</td><td class="assumption">ASSUMED – to be confirmed by measured geometry and natural period [I1]</td></tr>
<tr><td>Effective conduit length</td><td>{geometry.effective_conduit_length:.1f} m</td><td>Calibrated reference value from the <a href="../../../src/digitalmodel/hydrodynamics/utube_tank.py">connected-tank model</a>; transfer is assumed</td><td class="assumption">ASSUMED – to be confirmed by measured geometry and natural period [I1]</td></tr>
<tr><td>Reference conduit-area input range</td><td>{min(d["areas"]):.1f}–{max(d["areas"]):.1f} m²</td><td>Reference period inputs; selected individual archived reductions are shown in Appendix D</td><td class="assumption">Reference inputs [I2]</td></tr></table>
<p>The illustrative conduit area lies inside the reference input range in the table. The calibrated
effective length is transferred as an assumption; a measured natural period for the new geometry is
required before predictive use, as described in the <a href="../../../src/digitalmodel/hydrodynamics/utube_tank.py">model calibration basis</a>.</p>
<p>At {d["area"]:.1f} m² the illustrative tank natural period is {MODEL.natural_period(leg_area=geometry.leg_area, conduit_area=d["area"], fill_depth=geometry.fill_depth, effective_conduit_length=geometry.effective_conduit_length):.1f} s; it is not tuned to the {p["vessel_period"]:.1f} s vessel period. The {geometry.effective_conduit_length:.1f} m effective length is an inertia parameter fitted to a reference natural period, rather than the physical {2*geometry.centroid_offset:.1f} m centroid spacing; its transfer to this geometry is assumed.</p>
{schematic()}
<h3>3.3 Environmental data</h3><p>No wave spectrum is specified. Fluid density is {MODEL.DEFAULT_DENSITY:g} kg/m³ and gravity is {MODEL.DEFAULT_GRAVITY:g} m/s², ASSUMED from the reference calculation constants <a href="#ref-model">[I1]</a>.</p>
<h3>3.4 Vessel data</h3><p>Vessel roll period {p["vessel_period"]:.1f} s, hull damping {p["hull_damping"]*100:.1f} % and alpha {p["authority"]*100:.1f} % are ASSUMED from the reference response inputs <a href="#ref-tests">[I2]</a>. Dimensional vessel calibration is not established.</p>
<h3>3.5 Criteria</h3><p>The reference ±{v(band, 0)} % natural-period band is an archived comparison criterion, not a design acceptance limit <a href="#ref-evidence">[I3]</a>. The moment-fit applicability is stated in <a href="#limits">Section 7</a>; no operating envelope is established.</p>
<h3>3.6 Load cases</h3><p>The conduit-area and forcing-period sweeps use the inputs above. Imposed roll {p["roll_degrees"]:.1f}°, damping forcing period {p["damping_period"]:.1f} s and level-difference amplitudes {" and ".join(f"{level:.1f} m" for level in p["levels"])} are ASSUMED reference conditions <a href="#ref-tests">[I2]</a>. For the separate single-tank comparison, mass ratio μ = {d["mu"]} and tuning ratio {d["tuning"]:.1f} are ASSUMED comparison inputs <a href="#ref-response">[I5]</a>. The complete sweep matrix is in <a href="#sensitivity">Appendix A</a>.</p>
<h2 id="method">4 Analysis methodology</h2>
<p>Exchange amplitude is the volume displaced between the legs about its mean (m³). Damping uses the equivalent viscous ratio ζ = c_eq/(2√(km)), with mass m, stiffness k and equivalent damping coefficient c_eq; a percentage displays 100ζ. Quadratic loss makes this equivalent ratio depend on the forcing amplitude and frequency. The conduit-loss coefficient term scales as A^−n, where A is conduit area and n is the fitted loss exponent; the full damping expression also includes area-dependent inertia. In phase denotes the moment component aligned with imposed roll; quadrature denotes the component 90° out of phase with it. The moment-fit band is the empirical forcing-period range of 20 s and longer with the stated moment RMS error; it does not qualify other inputs.</p>

<p>The connected-tank model and the tuned-absorber model are evaluated at the illustrative inputs in Section 3. The archived CFD evidence is reported separately: the evidence record gives the source reduction, field and digest of each value, and raw fields and the archived geometry are excluded. The analytical scope follows the single-tank sloshing study; the connected-tank model is calibrated on its own.</p>
<p>The rigid-column energy balance provides the exchange natural period; quadratic conduit loss gives amplitude-dependent damping. Harmonic moment balance supplies the tank moment and coupled roll response. Frozen-fluid inertia is the roll inertia when the fluid is held fixed relative to the tank; it integrates the two prismatic legs and conduit; the separate tuned absorber illustrates peak splitting <a href="#ref-model">[I1]</a> <a href="#ref-reduction">[I4]</a>.</p>
<p>Calculations use digitalmodel {software_version()}. The analytical implementation and archived CFD software-version gap are identified in <a href="#ref-software">[I6]</a>.</p>
<p>RMS denotes root mean square error; GCI denotes grid convergence index. The Courant number is the local fluid travel distance per time step divided by cell size, Co = |u|Δt/Δx. Richardson order p describes how numerical error scales with grid spacing, error ∝ Δx^p; an observed low order and non-asymptotic convergence limit the GCI interpretation.</p>
<h2 id="curves">5 Results</h2>
<p>Sections 5.1–5.3 report illustrative continuous model evaluations; Section 5.4 summarizes the separate discrete archived observations in Appendix D. Each figure provides its source reference, hover values and a CSV export.</p>
<h3>5.1 Coupled roll response</h3>

<p>With the tank, the predicted peak roll falls from {bare_peak:.1f} to {tank_peak:.1f} per unit wave moment (vessel roll period {p["vessel_period"]:.1f} s, hull damping {p["hull_damping"]*100:.1f} %, alpha {p["authority"]*100:.1f} %); this is a screening prediction for the invented system.</p>
<p>The coupled response is evaluated with the <a href="../../../tests/hydrodynamics/test_utube_tank.py">reference values from the study’s test inputs</a>:
vessel roll period {p["vessel_period"]:.1f} s, hull damping {p["hull_damping"]*100:.1f} % and
alpha {p["authority"]*100:.1f} %. Alpha denotes the normalized tank stiffness scale, distinct
from a mass ratio. In the <a href="../../../src/digitalmodel/hydrodynamics/utube_tank.py">coupled-response model</a>,
the forcing reference is fixed by the bare-vessel roll amplitude. The calculated response is roll per
unit wave moment at that finite reference; quadratic loss makes it amplitude-dependent. Shading marks
periods outside the reference moment-fit band. Wave-spectrum and dimensional vessel calibration are
not established.</p>
<p>Roll per unit wave moment means |θ|/(M_wave/K_hull), with roll θ in radians and normalized K_hull = 1. The bare response at its natural period is 1/(2ζ_h) = {1/(2*p["hull_damping"]):.1f} for hull damping ratio ζ_h = {p["hull_damping"]:.2f}. The finite forcing reference is M_wave/K_hull = 2ζ_h × (5π/180), giving 5° bare roll at resonance.</p>
{svg_roll}
<h3>5.2 Illustrative response curves</h3>

<p>The connected-tank curves are evaluated using the <a href="../../../src/digitalmodel/hydrodynamics/utube_tank.py">analytical model</a>
at the illustrative inputs listed above. Frozen-fluid prismatic roll inertia is calculated in the inertia
curve; a validation error for rigid-column conduit inertia is not established by that curve.</p>
{svg_period}{svg_inertia}
<p>Damping is evaluated at forcing period {p["damping_period"]:.1f} s with imposed level-difference amplitudes
from the <a href="../../../tests/hydrodynamics/test_utube_tank.py">reference damping inputs</a>.
The calculated damping depends on amplitude; it is not a fixed tank property. The largest plotted equivalent damping is {100*max(max(values) for values in d["damping"]):.1f} %: over-critical (above 100 %). It is an amplitude-dependent equivalent loss ratio, not a constant linear free-decay damping ratio. Percent axes divide by 100 to give the dimensionless ratio used in the archived comparisons.</p>{svg_damping}
<p>The moment is calculated at {p["roll_degrees"]:.1f}° imposed roll and the illustrative conduit area,
using the <a href="../../../src/digitalmodel/hydrodynamics/utube_tank.py">tank-moment model</a>.</p>{svg_moment}
<h3 id="comparison">5.3 Single-tank comparison: split and merging</h3>

<p>The <a href="../../../scripts/cfd/reduce_tld_two_peak.py">tuned-absorber model</a> is evaluated at the
frequency grid and damping values in the <a href="../structural/sloshing-tld-twopeak.json">reference response record</a>.
The mass ratio μ = {d["mu"]} and frequency tuning ratio {d["tuning"]:.1f} are ASSUMED comparison inputs.
A single equivalent tank is coupled to an undamped primary vessel. The tank is not isolated, and its
geometry is not matched to the connected-tank system.</p>
<p>Peak normalized roll is the maximum of |θ|/(M_wave/K_hull) over the plotted frequency-ratio grid: roll amplitude divided by the static roll deflection under the same wave moment. The single-tank comparison uses an undamped primary vessel; its damping ratios use the same ζ convention.</p>
{svg_split}<table><tr><th>Tank damping (%)</th><th>Resolved peaks</th><th>Peak normalized roll</th></tr>{absorber_rows}</table>
<p>Two peaks are resolved in the light-damping curves; they merge as damping increases, and the peak
rises again at the highest damping shown in the table. These curves are evaluated using the
<a href="../../../scripts/cfd/reduce_tld_two_peak.py">single-tank model</a> and the
<a href="../structural/sloshing-tld-twopeak.json">reference response record</a>. New connected-tank CFD
results are not included. The connected-tank curves are presented separately.</p>
<h3>5.4 Archived CFD comparisons</h3>
<p>The separate archived campaign supports screening comparisons of exchange amplitude, natural period and equivalent damping. Four of five off-calibration period checks meet the stated band; the low-fill check exceeds it. The loss exponent is calibrated on 35 cases, with an independent intermediate-area check and a separate 38-case refit. Selected forced-roll and numerical checks retain their stated limits. The individual archived reductions, figures and tables appear in <a href="#archive">Appendix D</a>; they do not validate the illustrative geometry.</p>
<h2 id="validation">6 Validation and benchmark</h2>

<p>The calibration quantities below are reported in the <a href="../../../src/digitalmodel/hydrodynamics/utube_tank.py">model calibration summary</a>
and <a href="../../../tests/hydrodynamics/test_utube_tank.py">reference calibration checks</a>.
RMS denotes root mean square error; each percentage applies to the corresponding reference fit. The loss coefficient and exponent are fitted on 35 archived cases. One archived natural-period case calibrates the effective conduit length, so its period agrees by construction. The intermediate-area prediction is evaluated without refitting; the separate 38-case refit includes that area and updates the loss coefficient and exponent. Selected individual archived reductions are shown in Appendix D; the aggregate fit statistics do not imply that all case-level measurements are reproduced.</p>
<table><tr><th>Reference quantity</th><th>Value</th></tr>{claim_rows}</table>
<p>The loss comparison is based on a separate refit of each area law, as documented in the
<a href="../../../tests/hydrodynamics/test_utube_tank.py">calibration checks</a>. A fixed-coefficient
exponent substitution is excluded. Effective conduit length is calibrated against a measured natural
period, as specified in the <a href="../../../src/digitalmodel/hydrodynamics/utube_tank.py">calibration basis</a>;
the forced-sweep peak is loss-controlled and varies with roll amplitude. The reported factor {c["length_inflation"]} is the effective length inferred from that forced-sweep peak divided by the length calibrated from the measured natural period <a href="#ref-model">[I1]</a>.</p>
<p>The archived CFD-derived periods and damping reductions are the completed comparison basis. They do not establish physical validation of the invented geometry. Validation of that geometry is planned against measured exchange natural period, moment and loss across roll amplitudes; those data are not supplied.</p>
<h2 id="limits">7 Limitations</h2>
<p>These results support screening only; design qualification is not established. The limitations below govern interpretation of <a href="#curves">Section 5</a>.</p>
<ul>
<li>The moment fit holds for forcing periods of {c["moment_min_period"]} s and longer ({c["moment_rms"]} % RMS) and degrades below about {c["degrades_below"]} s; the cause is not established. See <a href="#sensitivity">Appendix A</a>.</li>
<li>Calibration transfer to the invented geometry remains an assumption even inside that band; a measured natural period is required before effective-length recalibration. See <a href="#geometry">Section 3</a> and <a href="#sensitivity">Appendix A</a>.</li>
<li>The single-tank comparison does not match mass, tuning, hull damping and forcing; only peak splitting and merging are illustrated. See <a href="#comparison">Section 5.3</a>.</li>
<li>Continuous fill coverage, trustworthy anti-roll phase and anti-roll benefit remain unestablished. See <a href="#cfd-fill">Appendix D.3</a> and <a href="#cfd-classes">Appendix A</a>.</li>
<li>Archived period checks do not establish a frozen-fluid roll-inertia error bound; wave-spectrum and dimensional coupling evidence is absent. See <a href="#sensitivity">Appendix A</a>.</li>
<li>Fine-mesh confirmation applies only at the selected period; its Courant value exceeds the nominal target, and free-decay convergence is non-asymptotic with indicative GCI only. See <a href="#cfd-forced">Appendix D.5</a>.</li>
</ul>
<h2 id="forward">8 Conclusions and next steps</h2>

<p>Further assessment is required before the illustrative response is applied to a design. The
<a href="#geometry">geometry assumptions</a>, <a href="#comparison">comparison limits</a> and
<a href="#limits">calibration limits</a> define the outstanding work below.</p><ul class="fw">
<li>Not done: The new geometry's natural period requires measurement before effective conduit length is recalibrated.</li>
<li>Not done: Moment and loss require validation across roll amplitudes and the short-period band.</li>
<li>Not done: Single and connected tanks require comparison at matched mass, tuning, hull damping and forcing.</li>
<li>Not done: The residual cause requires resolution before the empirical validity band is widened.</li>
<li>Not done: Operating fill, freeboard, structural loads and dimensional vessel coupling require assessment.</li></ul>
<p>The low-fill exception to the reference period band requires natural-period measurement and recalibration before predictive use; the affected case and error appear in <a href="#period-error">Figure D.2</a>. The comparison supports the stated analytical scope and does not qualify the invented illustrative geometry.</p>
<h2 id="basis">9 References</h2>
<p>The following register separates the model and data records from method and software references. Evidence locators identify the archived reductions without including their raw fields or geometry.</p>
{reference_sections(specs)}
<h2 id="sensitivity">Appendix A – Sensitivity matrix</h2>
<p>The matrix identifies which input changes each result and where evidence is missing. It introduces no new sensitivity calculation.</p>
{ sensitivity_matrix(d) }
<details><summary>Evidence classes and supporting limitations</summary>
<p>An End marker is the normal solver-completion message. Open transfer denotes the open-boundary transfer check, rejected because of boundary-flux growth. A screening manifest is a list of analytical screening cases; it does not establish completed CFD solutions <a href="#ref-evidence">[I3]</a>.</p>
{evidence_classes()}

<p>Screening evidence only; the qualification limits are stated in <a href="#limits">Section 7</a>. Raw CFD fields, meshes, the archived geometry are omitted.</p>
</details>
<h2 id="reproduce">Appendix B – Data and source notes</h2>
{source_appendix()}
{revision_history()}
{render_cfd_evidence()}
<footer>Illustrative analytical predictions and separate archived CFD evidence; transfer remains unqualified.</footer></div>{TOOLTIP_SCRIPT}</body></html>
'''
    return number_objects(localize_references(page))


def build(output=OUTPUT):
    started = perf_counter()
    output = Path(output)
    output.write_text(render(), encoding="utf-8", newline="\n")
    chart_directory = output.parent / 'data/charts'
    chart_directory.mkdir(parents=True, exist_ok=True)
    for spec in chart_specs():
        (chart_directory / (spec['id'] + '.csv')).write_text(csv_text(spec), encoding='utf-8', newline='\n')
    elapsed = perf_counter() - started
    print(f"Generated connected-tank study; wall time {elapsed:.3f} s")
    return elapsed


def build_review_copy(directory):
    """Write a portable copy with embedded styles and byte-identical data assets."""
    directory = Path(directory)
    data = directory / 'data'
    charts = data / 'charts'
    charts.mkdir(parents=True, exist_ok=True)
    page = render().replace(
        '<link rel="stylesheet" href="../_assets/brand.css">',
        '<style data-review-brand>' + (ROOT / 'docs/api/_assets/brand.css').read_text() + '</style>',
    ).replace('href="../structural/sloshing-tld-twopeak.json"',
              'href="data/sloshing-tld-twopeak.json"')
    target = directory / 'connected-tank-review-copy.html'
    target.write_text(page, encoding='utf-8', newline='\n')
    for spec in chart_specs():
        (charts / (spec['id'] + '.csv')).write_text(csv_text(spec), encoding='utf-8', newline='\n')
    (data / EVIDENCE_PATH.name).write_bytes(EVIDENCE_PATH.read_bytes())
    (data / JSON_PATH.name).write_bytes(JSON_PATH.read_bytes())
    digest = hashlib.sha256(target.read_bytes()).hexdigest()
    (directory / 'SHA256.txt').write_text(f'{digest}  {target.name}\n', encoding='utf-8')
    return target


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--review-copy', type=Path, help='Also write a portable review copy to this directory')
    args = parser.parse_args()
    build()
    if args.review_copy:
        build_review_copy(args.review_copy)
