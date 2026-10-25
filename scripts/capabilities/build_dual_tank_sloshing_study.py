# ABOUTME: Build a source-neutral connected-tank analysis from public models.
"""Regenerate the connected-tank page using committed analytical sources and sanitized CFD evidence.

Run: python scripts/capabilities/build_dual_tank_sloshing_study.py
No solver, network access or third-party package is required. The model module
is loaded by file so optional hydrodynamics package dependencies are unnecessary.
"""
from __future__ import annotations

import ast
import csv
import io
import textwrap
import html
import importlib.util
import json
import math
from pathlib import Path
import re
import sys
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
                 'Forcing period (s)', 'Roll per unit wave moment (normalized, no units)',
                 's', 'no units', [response_source] * 2, True,
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
             'Exchange natural period against conduit area (archived twin-tank CFD, 50 % fill)',
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
    c3 = add('period-error', 'Calibrated period error against the published ±3 % band',
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
            'Forcing / vessel natural frequency (no units)', 'Roll / static deflection (no units)',
            'no units', 'no units', [REDUCTION_PATH.relative_to(ROOT).as_posix() + '#_response; ' +
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
                                        ('linear_prediction', 'predicted_damping_no_units', 'Predicted')],
                   'Fill (%)', 'Damping ratio (no units)', '%', 'no units')
    loss = sorted(area_checks, key=lambda r: val(r['area']))
    near = [dict(area=r['area'], **min(r['damping'], key=lambda c:
                abs(val(c['period']) - val(r['predicted_period'])))) for r in loss]
    evidence_chart('cfd-loss-area', 'Equivalent damping against conduit area (archived CFD geometry)', near,
                   ('area', 'area_m2'), [('measured', 'cfd_damping_no_units', 'CFD-derived, near predicted natural period'),
                                        ('linear_prediction', 'inverse_area_damping_no_units', 'Original inverse-area prediction')],
                   'Conduit area (m²)', 'Damping ratio (no units)', 'm²', 'no units')
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
    attrs = f'stroke="{color}" stroke-width="1.6"'
    if shape == 'diamond':
        return f'<polygon points="{x:.2f},{y-5:.2f} {x+5:.2f},{y:.2f} {x:.2f},{y+5:.2f} {x-5:.2f},{y:.2f}" fill="var(--panel)" {attrs}/>'
    if shape == 'square':
        return f'<rect x="{x-4:.2f}" y="{y-4:.2f}" width="8" height="8" fill="{color}" {attrs}/>'
    if shape == 'cross':
        return (f'<line x1="{x-4:.2f}" y1="{y-4:.2f}" x2="{x+4:.2f}" y2="{y+4:.2f}" {attrs}/>'
                f'<line x1="{x-4:.2f}" y1="{y+4:.2f}" x2="{x+4:.2f}" y2="{y-4:.2f}" {attrs}/>')
    return f'<circle cx="{x:.2f}" cy="{y:.2f}" r="4" fill="{color}" {attrs}/>'


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
        parts.append(svg_text(12, legend_y, 'outside the published moment-fit band', fill='var(--ink)'))
        legend_y += 36
    if categories:
        parts.append(svg_text(12, legend_y, f'Published ±{spec["tolerance"]:.1f} % band', fill='var(--ink)'))
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
            both = (f"; Bare vessel {row['roll_bare_per_unit_moment']:.6g} no units; "
                    f"With connected tank {row['roll_with_tank_per_unit_moment']:.6g} no units"
                    if identifier == 'coupled-response' else '')
            yvalue = f"{point['y']:.1f}" if spec['yunit'] == '%' else f"{point['y']:.6g}"
            tooltip = (f"{series['name']}; {case}; {spec['ylabel']}: {yvalue} {spec['yunit']}{fill}; "
                       f"{role}; {comparison}{both}; {point['source']}").replace('−', '-')
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
    sources = ''.join('<li>' + html.escape(s['name']) + ': <code>' +
                      html.escape(' | '.join(dict.fromkeys(p['source'] for p in s['points']))) + '</code></li>'
                      for s in spec['series'])
    return (f'<figure class="chart"><figcaption>{html.escape(spec["title"])}</figcaption>'
            f'<svg id="{identifier}" class="study-chart" viewBox="0 0 360 {legend_y+12}" role="img" aria-labelledby="{identifier}-title {identifier}-desc">'
            f'<title id="{identifier}-title">{html.escape(spec["title"])}</title>'
            f'<desc id="{identifier}-desc">{html.escape(desc)}</desc>' + ''.join(parts) + '</svg>'
            f'<p><a href="data/charts/{identifier}.csv">Data (CSV)</a></p><ul class="sources">{sources}</ul></figure>')


def schematic():
    """Draw the illustrative transverse section from the same input constants."""
    width = ILLUSTRATIVE_LEG_WIDTH * 8
    offset = ILLUSTRATIVE_CENTROID_OFFSET * 8
    depth = ILLUSTRATIVE_FILL_DEPTH * 18
    left, right = 180-offset, 180+offset
    base = 240
    effective = MODEL.UTubeGeometry(
        leg_length=ILLUSTRATIVE_LEG_LENGTH, leg_width=ILLUSTRATIVE_LEG_WIDTH,
        fill_depth=ILLUSTRATIVE_FILL_DEPTH, centroid_offset=ILLUSTRATIVE_CENTROID_OFFSET,
    ).effective_conduit_length
    labels = [
        f'Each leg width {ILLUSTRATIVE_LEG_WIDTH:.1f} m; centroids ±{ILLUSTRATIVE_CENTROID_OFFSET:.1f} m from centreline',
        f'Fill depth {ILLUSTRATIVE_FILL_DEPTH:.1f} m; free surfaces tilted: sketch, not a computed state',
        f'conduit, area A_c = {ILLUSTRATIVE_CONDUIT_AREA:.1f} m², effective length {effective:.1f} m (calibrated reference, transfer assumed)',
        f'Leg length {ILLUSTRATIVE_LEG_LENGTH:.1f} m into the page; tank height not specified',
        'archived CFD twin-tank geometry is not shown',
    ]
    parts = ['<svg id="schematic" viewBox="0 0 360 650" role="img" aria-labelledby="schematic-title schematic-desc">',
             '<title id="schematic-title">Illustrative U-tube system, transverse section</title>',
             '<desc id="schematic-desc">' + html.escape('; '.join(labels)) + '</desc>']
    for center, shift, name in ((left, -14, 'Left leg'), (right, 14, 'Right leg')):
        parts.append(f'<rect x="{center-width/2}" y="75" width="{width}" height="{base-75}" fill="var(--panel)" stroke="var(--navy)" stroke-width="2"/>')
        surface = base-depth+shift
        parts.append(f'<polygon points="{center-width/2},{surface-5} {center+width/2},{surface+5} {center+width/2},{base} {center-width/2},{base}" fill="var(--teal)" opacity=".2"/>')
        parts.append(f'<line x1="{center-width/2}" x2="{center+width/2}" y1="{surface-5}" y2="{surface+5}" stroke="var(--teal)" stroke-width="2"/>')
        parts.append(svg_text(center, 60, name, text_anchor='middle'))
        parts.append(svg_text(center, surface-15, 'free surface', text_anchor='middle'))
    parts.append(f'<rect x="{left+width/2}" y="{base-ILLUSTRATIVE_CONDUIT_AREA*2}" width="{right-left-width}" height="{ILLUSTRATIVE_CONDUIT_AREA*2}" fill="var(--teal)" fill-opacity=".2" stroke="var(--navy)" stroke-width="2"/>')
    parts.append(f'<line x1="180" x2="180" y1="{base-depth-14}" y2="{base-depth+14}" stroke="var(--ink)"/>')
    parts.append(svg_text(188, base-depth+4, 'Δh'))
    parts.extend(['<line x1="180" x2="180" y1="72" y2="288" stroke="var(--mut)" stroke-dasharray="4 4"/>',
                  '<circle cx="180" cy="280" r="4" fill="var(--navy)"/>',
                  '<path d="M 150 280 A 30 30 0 0 1 210 280" fill="none" stroke="var(--navy)" stroke-width="2"/>',
                  '<polygon points="205,274 210,280 214,272" fill="var(--navy)"/>',
                  svg_text(180, 310, 'roll angle φ', text_anchor='middle')])
    y = 350
    for label in labels:
        parts.append(svg_text(12, y, label, wrap=44))
        y += len(textwrap.wrap(label, 44)) * 16 + 14
    parts.append('</svg>')
    return ('<figure>' + ''.join(parts) + '<figcaption>Schematic of the invented, illustrative U-tube system (transverse section; not to scale vertically). It represents no particular vessel.</figcaption></figure>')


LAYOUT_CSS = """
figure{margin:22px 0;min-width:0}svg{width:100%;height:auto;display:block;color:var(--ink);font:inherit;font-size:13px;fill:var(--ink)}
figcaption{font-size:1rem;color:var(--navy)}.sources{font-size:.8rem;padding-left:20px}.sources code{overflow-wrap:anywhere}
.chart{border:1px solid var(--grid);border-radius:6px;padding:8px;background:var(--panel)}
.chart g[tabindex]:focus{outline:2px solid var(--accent)}nav{overflow-wrap:anywhere}
table{table-layout:fixed}td,th{overflow-wrap:anywhere}#chart-tooltip{position:fixed;bottom:10px;left:10px;max-width:calc(100vw - 20px);width:320px;padding:12px;background:var(--panel);color:var(--ink);border:1px solid var(--navy);z-index:2;overflow-wrap:anywhere;font-size:.85rem}
@media(max-width:600px){.wrap{padding-left:8px;padding-right:8px}.chart{padding:4px}th,td{padding:5px 3px;font-size:.75rem}}
"""

TOOLTIP_SCRIPT = """<script>
(() => {
  const box = document.createElement('div');
  box.id = 'chart-tooltip'; box.setAttribute('role', 'status'); box.hidden = true;
  document.body.append(box);
  const show = event => {
    const point = event.target.closest('[data-tooltip]');
    box.hidden = !point;
    if (point) box.textContent = point.dataset.tooltip;
  };
  for (const name of ['pointerover', 'focusin', 'pointerdown']) document.addEventListener(name, show);
  document.addEventListener('keydown', event => { if (event.key === 'Escape') box.hidden = true; });
  document.addEventListener('focusout', () => { box.hidden = true; });
  document.addEventListener('pointerout', event => {
    if (event.pointerType !== 'touch') box.hidden = true;
  });
})();
</script>"""


def format_percentage_text(value):
    """Round percentage prose, preserving other measurements and integer counts."""
    def rounded(match):
        display = f"{float(match[1]):.1f}"
        if match[3] is not None:
            display += f"{match[2]}{float(match[3]):.1f}"
        return display + " %"

    return re.sub(
        r"(?<![\w.])([+-]?\d+(?:\.\d+)?)(?:\s*([-–])\s*([+-]?\d+(?:\.\d+)?))?\s*%",
        rounded, value,
    )


def evidence_value(fact, precision=1):
    """Bind a visible scalar to its raw evidence value and source pointer."""
    value = fact["value"]
    display = (f"{value:.{precision}f}" if isinstance(value, (int, float))
               else format_percentage_text(str(value)))
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
    part = ['<section><h2 id="cfd-evidence">CFD evidence</h2>',
            '<p>The CFD cases belong to an earlier, archived campaign on a twin-tank geometry that is not shown. '
            'That geometry is separate from the invented illustrative geometry above. These CFD results were not '
            'produced for this page. The <a href="data/dual_tank_cfd_evidence.json">evidence record</a> '
            'identifies the source reduction, field and digest for each value. '
            'Raw fields and geometry are excluded.</p>',
            '<h3 id="cfd-area">Conduit-area sweep</h3>',
            f'<p>Exchange amplitudes are compared at forcing period {v(conditions["period"], 2)} s '
            f'and imposed roll {v(conditions["roll"])}°. Ratios are normalized to the smallest area. '
            'The historical analytical amplitude uses geometric conduit length; it is not the later calibrated model.</p>']
    part.append(charts["cfd-amplitude"])
    part.append(table(["Area (m²)", "CFD amplitude (m³)", "Analytical amplitude (m³)", "CFD ratio", "Analytical ratio"],
                      [[v(r["area"]), v(r["cfd_amplitude"]), v(r["analytical_amplitude"]),
                        v(r["cfd_ratio"], 3), v(r["analytical_ratio"], 3)] for r in rows],
                      "Exchange-amplitude comparison; ratios retain the archived precision."))
    pairs = area_period_pairs(d)
    measured = [m for _, m, _ in pairs]
    predicted = [p for _, _, p in pairs]
    part.append('<p><a href="#natural-period-vs-area">Exchange natural period against conduit area (archived twin-tank CFD, 50 % fill)</a></p>')
    part.append(table(["Area (m²)", "CFD-derived natural period (s)", "Calibrated prediction (s)", "Historical geometric-length prediction (s)"],
                      [[v(r["area"]), v(m), v(p), v(r["historical_period"])] for r, m, p in pairs],
                      "Period bases differ: the historical period is analytical, not a CFD measurement. The middle area is the calibration reference."))
    band = d["published_band"]
    part += ['<h3 id="cfd-inertia">Inertia and natural period</h3>',
             f'<p>The archive publishes a ±{v(band, 0)} % band for the inertia law using one calibrated effective conduit length. '
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
    part += ['<h3 id="cfd-fill">Fill sweep</h3>',
             '<p>Discrete low and high fill checks bracket the calibration fill; continuous fill coverage is not established. '
             'Equivalent damping depends on forcing period and imposed roll. The table retains each available condition; '
             'the damping chart selects the lower-roll case nearest each predicted natural period.</p>']
    chosen = [min(r["damping"], key=lambda c: (val(c["roll"]), abs(val(c["period"]) - val(r["predicted_period"])))) for r in fill]
    fill_labels = [f'{val(r["fill"]):.0f}' for r in fill]
    part.append(charts["cfd-fill-period"])
    part.append(charts["cfd-fill-damping"])
    part.append(table(["Fill (%)", "Area (m²)", "Forcing period (s)", "Roll (°)", "Equivalent damping ratio"],
                      [[v(r["fill"], 0), v(r["area"]), v(c["period"]), v(c["roll"]), v(c["measured"], 3)] for r in fill for c in r["damping"]],
                      "Fill-dependent damping at the archived forcing conditions; period predictions and measurements appear above."))
    loss = d["loss"]
    third = loss["third"]
    part += ['<h3 id="cfd-loss">Loss scaling and independent area check</h3>',
             f'<p>The fitted area exponent is n = {v(loss["exponent"], 3)}, with {v(loss["rms"])} % RMS '
             f'over {v(loss["cases"], 0)} cases. The naive inverse-area form has '
             f'{v(loss["linear_rms"])} % RMS after its own coefficient refit: it underpredicts damping at the smallest area '
             f'(group RMS {v(loss["small_area_rms"])} %) and overpredicts at the largest '
             f'(group RMS {v(loss["large_area_rms"])} %). These group RMS magnitudes are distinct from signed individual errors.</p>']
    # Use matched near-natural-period conditions, not an unqualified average of the loss-fit surface.
    loss_checks = sorted([r for r in checks if val(r["fill"]) == val(calibration["fill"])], key=lambda r: val(r["area"]))
    near = [min(r["damping"], key=lambda c: abs(val(c["period"]) - val(r["predicted_period"]))) for r in loss_checks]
    part.append(charts["cfd-loss-area"])
    part.append(table(["Area (m²)", "Fill (%)", "Forcing period (s)", "Roll (°)", "CFD-derived damping", "Original inverse-area damping"],
                      [[v(r["area"]), v(r["fill"], 0), v(c["period"]), v(c["roll"]), v(c["measured"], 3), v(c["linear_prediction"], 3)] for r, c in zip(loss_checks, near)],
                      "Conditions near each predicted natural period. The original inverse-area prediction uses the reference calibration, not the separate all-data coefficient refit."))
    part += [f'<p>At the independent intermediate area, the existing law evaluated without refitting gives '
             f'{v(third["independent_rms"])} % RMS. Including that area in the fit gives '
             f'n = {v(third["exponent"], 3)} and {v(third["rms"])} % RMS over '
             f'{v(third["cases"], 0)} cases. Percentages are rounded to one decimal place; '
             'the evidence record retains the source precision. '
             'The independent prediction and all-data refit are separate checks.</p>',
             '<h3 id="cfd-forced">Forced-roll period sweep and numerical checks</h3>',
             '<p>The medium-mesh forced-roll cases are accepted for screening after cycle convergence. '
             'The exchange plateau is separate from the exchange natural period. Fine-mesh confirmation applies at the selected period only; '
             'it does not establish convergence throughout the sweep.</p>']
    sweep = d["forced_sweep"]
    part.append(charts["cfd-forced-exchange"])
    part.append(table(["Forcing period (s)", "Exchange amplitude (m³)", "Roll-moment amplitude (N m)"],
                      [[v(r["period"], 0), v(r["exchange"]), v(r["moment"])] for r in sweep],
                      "Completed, cycle-converged medium-mesh sweep."))
    mesh, frequency = d["mesh"], d["frequency"]
    part.append(table(["Numerical check", "Result"], [
        ["Fine confirmation period (s)", v(mesh["period"], 0)],
        ["Exchange change (%)", v(mesh["exchange_error"])],
        ["Moment change (%)", v(mesh["moment_error"])],
        ["Fine transient maximum Courant / nominal target", v(mesh["max_courant"], 3) + " / " + v(mesh["courant_target"])],
        ["Free-decay frequency error (%)", v(frequency["minimum_error"]) + "–" + v(frequency["maximum_error"])],
        ["Observed Richardson order", v(frequency["richardson_order"], 3)],
        ["Indicative fine-grid GCI (%)", v(frequency["gci"])],
    ], "Fine-mesh Courant exceeds the nominal target. Free-decay convergence is non-asymptotic; the GCI is indicative. Free-decay validates frequency in its own case family, not twin-tank anti-roll response."))
    part += ['<h3 id="cfd-classes">Evidence classes and withheld conclusions</h3>',
             '<p>Analytical screening cases are not CFD solves. Failed wrappers and open transfer are rejected. '
             'Short transfer checks are verification only. Trustworthy phase and anti-roll benefit remain withheld.</p>']
    classes = d["evidence_classes"]
    part.append(table(["Evidence class", "Result family", "Disposition", "Finding", "Limitation"],
                      [[v(r[key]) for key in ("evidence_class", "result_family", "status", "key_finding", "limitation")] for r in classes],
                      "The source-neutral review controls the evidence classification; methodology and validation are not interchangeable."))
    return "\n".join(part) + "\n</section>"


def render():
    d = compute()
    c = published_claims()
    p, geometry = d["inputs"], d["geometry"]
    style = SHOWCASE._CSS + LAYOUT_CSS
    claims = (
        ("Quadratic conduit loss: calibrated area exponent", c["area_exponent"]),
        ('Calibration cases (aggregate results only)', c["cases"]),
        ("Calibrated loss-law RMS (%)", c["loss_rms"]),
        ("Refitted linear-in-area law RMS (%)", c["linear_rms"]),
        ("Closed-form moment RMS (%)", c["moment_rms"]),
        ("Moment fit applies from forcing period (s)", c["moment_min_period"]),
        ("Degradation reported below about (s)", c["degrades_below"]),
        ("Effective-length inflation from forced-sweep peak (times)", c["length_inflation"]),
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
    return f'''<!DOCTYPE html>
<html lang="en" data-theme="light"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Connected-tank sloshing analysis</title>
<link rel="stylesheet" href="../_assets/brand.css"><style>{style}</style></head>
<body><div class="wrap"><header><div class="kicker">Hydrodynamics · analytical study</div>
<h1>Connected-tank sloshing analysis</h1>
<p class="sub">U-tube anti-roll tank: analytical screening model calibrated on archived CFD</p>
<div class="meta"><div><span>Analytical model</span></div><div><span>{v(loss["cases"], 0)} archived CFD cases (aggregate)</span></div><div><span>Period within ±{v(band, 0)} % at {inside} of {len(checks)} checks</span></div><div><span>Draft for maintainer review</span></div></div></header>
<nav><a href="#schematic">Schematic</a> · <a href="#summary">Summary</a> · <a href="#limits">Limits</a> ·
<a href="#revisions">Revision history</a> · <a href="#method">Method</a> · <a href="#geometry">Geometry</a> ·
<a href="#validation">Published validation</a> · <a href="#curves">Computed curves</a> · <a href="#comparison">Single-tank comparison</a> ·
<a href="#cfd-evidence">CFD evidence</a> · <a href="#forward">Way forward</a> · <a href="#left">Left</a> · <a href="#basis">Analysis basis</a></nav>
{schematic()}
<h2 id="summary">Summary</h2>
<p>This study is extended to assess a U-tube style anti-roll arrangement: two connected tanks with invented geometry, joined by a conduit. The geometry represents no particular vessel or client. The connected-tank model is an analytical screening model calibrated on CFD assessment: the aggregate results of 35 cases from an earlier, archived CFD campaign on a separate twin-tank geometry. Analysis is performed to determine the connected-tank response and how closely the calibrated model reproduces the archived CFD; the key results are summarized below.</p>
<ul><li>Conduit loss: a calibrated area exponent of {v(loss["exponent"], 3)} fits the archived damping with {v(loss["rms"])} % RMS over {v(loss["cases"], 0)} cases. The simple inverse-area law, refitted on its own, gives {v(loss["linear_rms"])} % RMS.</li>
<li>Natural period: the calibrated model reproduces the CFD-derived exchange natural period within the published ±{v(band, 0)} % band at {count_text} off-calibration checks. The low-fill check ({v(low_fill["fill"], 0)} %) is {v(low_fill["error"])} % high. The calibration case agrees by construction ({v(calibration["predicted_period"], 2)} s against {v(calibration["measured_period"], 2)} s).</li>
<li>Response: with the tank, the predicted peak roll of the invented system falls from {bare_peak:.1f} to {tank_peak:.1f} per unit wave moment. This is a screening prediction, not a design result.</li></ul>
<p>With the tank, the predicted peak roll falls from {bare_peak:.1f} to {tank_peak:.1f} per unit wave moment (vessel roll period {p["vessel_period"]:.1f} s, hull damping {p["hull_damping"]*100:.1f} %, tank authority {p["authority"]*100:.1f} %); this is a screening prediction for the invented system.</p>
{svg_roll}
<p>No fitted curve: the archived geometry is not published, so the model cannot be drawn between the cases from committed inputs.</p>
{charts["natural-period-vs-area"]}
<p>The calibration case is not plotted, because it is not an independent check.</p>
{charts["period-error"]}
<h2 id="limits">Limits</h2>
<p>These results support screening only; design qualification is not established. The moment fit holds for forcing periods of 20 s and longer (4.3 % RMS) and degrades below about 16 s; the cause is not established, and an earlier explanation was withdrawn. Transfer of the calibration to the invented geometry is an assumption, also inside that band; the new geometry's natural period must be measured before the effective conduit length is recalibrated. The single-tank comparison illustrates peak splitting and merging only: single and connected tanks are not compared at matched mass, tuning, hull damping and forcing. Continuous fill coverage, anti-roll phase and anti-roll benefit remain unestablished.</p>
<details><summary>Revision history</summary>
<h2 id="revisions">Revision history</h2>
<table><tr><th>Revision</th><th>Date</th><th>What changed and why</th></tr>
<tr class="revision-2"><td>2</td><td>2026-10-25</td><td>schematic added; three summary charts; every chart interactive with a CSV; the limits gathered in one place</td></tr>
<tr><td>Display precision</td><td>2026-10-23</td><td>Damping-axis labels distinguished and comparison percentages rounded to one decimal place; source precision retained in the evidence record.</td></tr>
<tr><td>Evidence</td><td>2026-10-23</td><td>Archived CFD reductions added with provenance and evidence-class limits to distinguish measurements from illustrative predictions.</td></tr>
<tr><td>Geometry</td><td>2026-10-23</td><td>Independent illustrative dimensions and conduit area selected to separate the predictions from the reference geometry.</td></tr>
<tr><td>Editorial</td><td>Not assigned</td><td>Engineering descriptions clarified to distinguish illustrative predictions from calibration evidence.</td></tr>
<tr><td>Initial</td><td>Not assigned</td><td>Public-source analysis added to document model scope and remaining work.</td></tr></table>
</details>
<h2 id="method">Method</h2>
<p>The connected-tank model and the tuned-absorber model are evaluated at the illustrative inputs below. The archived CFD evidence is reported separately: the evidence record gives the source reduction, field and digest of each value, and raw fields and the archived geometry are excluded. The analytical scope follows the single-tank sloshing study; the connected-tank model is calibrated on its own.</p>
<h2 id="geometry">Illustrative geometry</h2>
<p>Invented round dimensions are selected independently of the
<a href="../../../tests/hydrodynamics/test_utube_tank.py">published reference geometry</a>.
Validation of the illustrative plan areas and centroid offset is not established. Fill depth is an
input; a validated fill-percentage range is not established.</p>
<table><tr><th>Input</th><th>Illustrative value</th><th>Source and assumption</th></tr>
<tr><td>Each leg length × width</td><td>{geometry.leg_length:.1f} × {geometry.leg_width:.1f} m</td><td>Independent invented round dimensions</td></tr>
<tr><td>Each leg plan area</td><td>{geometry.leg_area:.1f} m²</td><td>Product of the illustrative leg length and width</td></tr>
<tr><td>Leg centroid offset from centreline</td><td>{geometry.centroid_offset:.1f} m</td><td>Independent invented round offset</td></tr>
<tr><td>Fill depth</td><td>{geometry.fill_depth:.1f} m</td><td>Independent invented round depth</td></tr>
<tr><td>Conduit area for period sweeps</td><td>{d["area"]:.1f} m²</td><td>Independent invented area within the published conduit-area input range</td></tr>
<tr><td>Effective conduit length</td><td>{geometry.effective_conduit_length:.1f} m</td><td>Calibrated reference value from the <a href="../../../src/digitalmodel/hydrodynamics/utube_tank.py">connected-tank model</a>; transfer is assumed</td></tr>
<tr><td>Published conduit-area input range</td><td>{min(d["areas"]):.1f}–{max(d["areas"]):.1f} m²</td><td>Reference period inputs; individual CFD measurements are not reproduced</td></tr></table>
<p>The illustrative conduit area lies inside the published input range in the table. The calibrated
effective length is transferred as an assumption; a measured natural period for the new geometry is
required before predictive use, as described in the <a href="../../../src/digitalmodel/hydrodynamics/utube_tank.py">model calibration basis</a>.</p>
<h2 id="validation">Published validation</h2>
<p>The calibration quantities below are reported in the <a href="../../../src/digitalmodel/hydrodynamics/utube_tank.py">model calibration summary</a>
and <a href="../../../tests/hydrodynamics/test_utube_tank.py">published calibration checks</a>.
RMS denotes root mean square error; each percentage applies to the corresponding reference fit.</p>
<table><tr><th>Published quantity</th><th>Value</th></tr>{claim_rows}</table>
<p>The loss comparison is based on a separate refit of each area law, as documented in the
<a href="../../../tests/hydrodynamics/test_utube_tank.py">calibration checks</a>. A fixed-coefficient
exponent substitution is excluded. Effective conduit length is calibrated against a measured natural
period, as specified in the <a href="../../../src/digitalmodel/hydrodynamics/utube_tank.py">calibration basis</a>;
the forced-sweep peak is loss-controlled and varies with roll amplitude.</p>
<h2 id="curves">Computed curves</h2>
<p>The connected-tank curves are evaluated using the <a href="../../../src/digitalmodel/hydrodynamics/utube_tank.py">analytical model</a>
at the illustrative inputs listed above. Frozen-fluid prismatic roll inertia is calculated in the inertia
curve; a validation error for rigid-column conduit inertia is not established by that curve.</p>
{svg_period}{svg_inertia}
<p>Damping is evaluated at forcing period {p["damping_period"]:.1f} s with imposed level-difference amplitudes
from the <a href="../../../tests/hydrodynamics/test_utube_tank.py">published damping inputs</a>.
The calculated damping depends on amplitude; it is not a fixed tank property.</p>{svg_damping}
<p>The moment is calculated at {p["roll_degrees"]:.1f}° imposed roll and the illustrative conduit area,
using the <a href="../../../src/digitalmodel/hydrodynamics/utube_tank.py">tank-moment model</a>.</p>{svg_moment}
<p>The coupled response is evaluated with the <a href="../../../tests/hydrodynamics/test_utube_tank.py">published response inputs</a>:
vessel roll period {p["vessel_period"]:.1f} s, hull damping {p["hull_damping"]*100:.1f} % and
tank authority {p["authority"]*100:.1f} %. Tank authority denotes a normalized stiffness scale, distinct
from a mass ratio. In the <a href="../../../src/digitalmodel/hydrodynamics/utube_tank.py">coupled-response model</a>,
the forcing reference is fixed by the bare-vessel roll amplitude. The calculated response is roll per
unit wave moment at that finite reference; quadratic loss makes it amplitude-dependent. Shading marks
periods outside the published moment-fit band. Wave-spectrum and dimensional vessel calibration are
not established.</p>
<p><a href="#coupled-response">Predicted roll response with and without the connected tank (illustrative system)</a></p>
<h2 id="comparison">Single-tank comparison: split and merging</h2>
<p>The <a href="../../../scripts/cfd/reduce_tld_two_peak.py">tuned-absorber model</a> is evaluated at the
frequency grid and damping values in the <a href="../structural/sloshing-tld-twopeak.json">published response record</a>.
The mass ratio, denoted by μ, is {d["mu"]}; the frequency tuning ratio is {d["tuning"]:.1f}.
A single equivalent tank is coupled to an undamped primary vessel. The tank is not isolated, and its
geometry is not matched to the U-tube system.</p>
{svg_split}<table><tr><th>Tank damping (%)</th><th>Resolved peaks</th><th>Peak normalized roll</th></tr>{absorber_rows}</table>
<p>Two peaks are resolved in the light-damping curves; they merge as damping increases, and the peak
rises again at the highest damping shown in the table. These curves are evaluated using the
<a href="../../../scripts/cfd/reduce_tld_two_peak.py">single-tank model</a> and the
<a href="../structural/sloshing-tld-twopeak.json">published response record</a>. New connected-tank CFD
results are not included. The connected-tank curves are presented separately.</p>
{render_cfd_evidence()}
<h2 id="forward">Way forward</h2>
<p>Further assessment is required before the illustrative response is applied to a design. The
<a href="#geometry">geometry assumptions</a>, <a href="#comparison">comparison limits</a> and
<a href="#limits">calibration limits</a> define the outstanding work below.</p><ul class="fw">
<li>Not done: The new geometry's natural period requires measurement before effective conduit length is recalibrated.</li>
<li>Not done: Moment and loss require validation across roll amplitudes and the short-period band.</li>
<li>Not done: Single and connected tanks require comparison at matched mass, tuning, hull damping and forcing.</li>
<li>Not done: The residual cause requires resolution before the empirical validity band is widened.</li>
<li>Not done: Operating fill, freeboard, structural loads and dimensional vessel coupling require assessment.</li></ul>
<h2 id="left">Left</h2>
<p>The archived period checks do not establish a direct frozen-fluid roll-inertia error bound.
Raw CFD fields, meshes, unstaged case quantities and the archived geometry are omitted. The earlier campaign does not qualify the invented illustrative geometry.</p>
<h2 id="basis">Analysis basis</h2>
<p>The analytical scope is described in the <a href="sloshing-cfd-study.html">sloshing study</a>.
The connected-tank calculations and aggregate calibration statistics are defined in the
<a href="../../../src/digitalmodel/hydrodynamics/utube_tank.py">analytical model</a> and
<a href="../../../tests/hydrodynamics/test_utube_tank.py">published calibration checks</a>.
The single-tank comparison is defined in the <a href="../../../scripts/cfd/reduce_tld_two_peak.py">tuned-absorber model</a>
and <a href="../structural/sloshing-tld-twopeak.json">response record</a>.</p>
<footer><a href="../../../scripts/capabilities/build_dual_tank_sloshing_study.py">Calculation basis for this study</a>.
Illustrative analytical predictions and separate archived CFD evidence; transfer remains unqualified.</footer></div>{TOOLTIP_SCRIPT}</body></html>
'''


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


if __name__ == "__main__":
    build()
