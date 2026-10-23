# ABOUTME: Build a source-neutral connected-tank analysis from public models.
"""Regenerate the connected-tank page using committed analytical sources and sanitized CFD evidence.

Run: python scripts/capabilities/build_dual_tank_sloshing_study.py
No solver, network access or third-party package is required. The model module
is loaded by file so optional hydrodynamics package dependencies are unnecessary.
"""
from __future__ import annotations

import ast
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


def chart(identifier, title, xs, series, xlabel, ylabel, supported_from=None):
    """Inline accessible SVG with independent axes, legend and validity shading."""
    colors = ("#0B3D91", "#0f8a7e", "#b8860b", "#943a78", "#555555")
    left, right, top, bottom = 84, 770, 46, 300
    xmin, xmax = min(xs), max(xs)
    ymin = min(0, min(min(ys) for _, ys in series))
    ymax = max(max(ys) for _, ys in series)

    def x(value):
        return left + (value - xmin) / (xmax - xmin) * (right - left)

    def y(value):
        return bottom - (value - ymin) / (ymax - ymin) * (bottom - top)

    shading = (" Shaded periods are outside the published moment fit band."
               if supported_from is not None else "")
    parts = [f'<svg id="{identifier}" viewBox="0 0 820 440" role="img" '
             f'aria-labelledby="{identifier}-title {identifier}-desc" '
             'style="width:100%;height:auto">',
             f'<title id="{identifier}-title">{html.escape(title)}</title>',
             f'<desc id="{identifier}-desc">{html.escape(ylabel)} against '
             f'{html.escape(xlabel)}. {html.escape("; ".join(n for n, _ in series))}.'
             f'{shading}</desc>',
             '<rect width="820" height="440" fill="white"/>',
             f'<text x="84" y="22" fill="#16324f">{html.escape(title)}</text>']
    if supported_from is not None:
        parts.append(f'<rect x="{left}" y="{top}" '
                     f'width="{x(supported_from)-left:.1f}" '
                     f'height="{bottom-top}" fill="#fff0d0"/>')
    for i in range(6):
        xv = xmin + (xmax - xmin) * i / 5
        yv = ymin + (ymax - ymin) * i / 5
        parts.extend([
            f'<path d="M {left} {y(yv):.1f} H {right}" stroke="#e2e7eb"/>',
            f'<text x="{left-8}" y="{y(yv)+4:.1f}" text-anchor="end" '
            f'font-size="12">{yv:.1f}</text>',
            f'<text x="{x(xv):.1f}" y="322" text-anchor="middle" '
            f'font-size="12">{xv:.1f}</text>',
        ])
    parts.extend([
        f'<text x="427" y="345" text-anchor="middle">{html.escape(xlabel)}</text>',
        '<text transform="translate(18 175) rotate(-90)" text-anchor="middle">'
        f'{html.escape(ylabel)}</text>',
    ])
    for index, (name, ys) in enumerate(series):
        color = colors[index % len(colors)]
        points = " ".join(f"{x(xv):.1f},{y(yv):.1f}" for xv, yv in zip(xs, ys))
        parts.extend([
            f'<polyline points="{points}" fill="none" stroke="{color}" stroke-width="2"/>',
            f'<text x="84" y="{365+index*16}" fill="{color}" '
            f'font-size="12">{html.escape(name)}</text>',
        ])
    return "\n".join(parts) + "\n</svg>"


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


def evidence_chart(identifier, title, labels, series, ylabel, tick_precision=1):
    """Use the study's SVG style with categorical cases and source-valued ticks.

    Axis ticks are selected from the observations; no interpolated data labels
    or inferred intermediate validation points are introduced.
    """
    colors = ("#0B3D91", "#0f8a7e", "#b8860b", "#943a78")
    values = [v for _, ys in series for v in ys]
    low, high = min(values), max(values)
    if high == low:
        high = low + 1
    ticks = sorted(set(values))
    if len(ticks) > 6:
        ticks = [ticks[round(i * (len(ticks) - 1) / 5)] for i in range(6)]
    def x(i):
        return 90 + i * 660 / max(1, len(labels) - 1)
    def y(v):
        return 300 - (v - low) * 245 / (high - low)
    parts = [f'<svg id="{identifier}" viewBox="0 0 820 440" role="img" '
             f'aria-labelledby="{identifier}-title {identifier}-desc" style="width:100%;height:auto">',
             f'<title id="{identifier}-title">{html.escape(title)}</title>',
             f'<desc id="{identifier}-desc">{html.escape(ylabel)}. '
             'Discrete archived cases; connecting lines guide the eye.</desc>',
             '<rect width="820" height="440" fill="white"/>',
             f'<text x="84" y="22" fill="#16324f">{html.escape(title)}</text>']
    for tick in ticks:
        parts += [f'<path d="M 84 {y(tick):.1f} H 770" stroke="#e2e7eb"/>',
                  f'<text x="76" y="{y(tick)+4:.1f}" text-anchor="end" font-size="12">{tick:.{tick_precision}f}</text>']
    for i, label in enumerate(labels):
        parts.append(f'<text x="{x(i):.1f}" y="322" text-anchor="middle" font-size="10">{html.escape(label)}</text>')
    parts.append('<text transform="translate(18 175) rotate(-90)" text-anchor="middle">'
                 f'{html.escape(ylabel)}</text>')
    for i, (label, ys) in enumerate(series):
        color = colors[i % len(colors)]
        points = " ".join(f"{x(j):.1f},{y(v):.1f}" for j, v in enumerate(ys))
        parts += [f'<polyline points="{points}" fill="none" stroke="{color}" stroke-width="2"/>',
                  f'<text x="84" y="{355+i*18}" fill="{color}" font-size="12">{html.escape(label)}</text>']
    return "\n".join(parts) + "\n</svg>"


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
    plot = evidence_chart
    table = evidence_table
    rows = d["area_sweep"]
    conditions = d["area_conditions"]
    checks = d["period_checks"]
    calibration = d["calibration"]
    areas = [str(val(r["area"])) for r in rows]
    part = ['<section id="cfd-evidence"><h2>CFD evidence</h2>',
            '<p>The CFD cases belong to an earlier, archived campaign on a twin-tank geometry that is not shown. '
            'That geometry is separate from the invented illustrative geometry above. These CFD results were not '
            'produced for this page. The <a href="data/dual_tank_cfd_evidence.json">evidence record</a> '
            'identifies the source reduction, field and digest for each value. '
            'Raw fields and geometry are excluded.</p>',
            '<h3 id="cfd-area">Conduit-area sweep</h3>',
            f'<p>Exchange amplitudes are compared at forcing period {v(conditions["period"], 2)} s '
            f'and imposed roll {v(conditions["roll"])}°. Ratios are normalized to the smallest area. '
            'The historical analytical amplitude uses geometric conduit length; it is not the later calibrated model.</p>']
    part.append(plot("cfd-amplitude", "Exchange amplitude against conduit area", areas,
                     [("CFD", [val(r["cfd_amplitude"]) for r in rows]),
                      ("Historical analytical", [val(r["analytical_amplitude"]) for r in rows])],
                     "Exchange amplitude (m³); conduit area (m²)"))
    part.append(table(["Area (m²)", "CFD amplitude (m³)", "Analytical amplitude (m³)", "CFD ratio", "Analytical ratio"],
                      [[v(r["area"]), v(r["cfd_amplitude"]), v(r["analytical_amplitude"]),
                        v(r["cfd_ratio"], 3), v(r["analytical_ratio"], 3)] for r in rows],
                      "Exchange-amplitude comparison; ratios retain the archived precision."))
    pairs = area_period_pairs(d)
    measured = [m for _, m, _ in pairs]
    predicted = [p for _, _, p in pairs]
    part.append(plot("cfd-area-period", "Exchange natural period against conduit area", areas,
                     [("CFD-derived natural period", list(map(val, measured))),
                      ("Calibrated inertia model", list(map(val, predicted)))], "Natural period (s); area (m²)"))
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
    part.append(plot("cfd-period-error", "Period prediction error: area / fill", labels,
                     [("Prediction error", [val(r["error"]) for r in checks]),
                      ("Published upper band", [val(band)] * len(checks)),
                      ("Published lower band", [-val(band)] * len(checks))], "Period error (%); area (m²) / fill (%)"))
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
    part.append(plot("cfd-fill-period", "Natural period against fill", fill_labels,
                     [("CFD-derived", [val(r["measured_period"]) for r in fill]),
                      ("Predicted", [val(r["predicted_period"]) for r in fill])], "Natural period (s); fill (%)"))
    part.append(plot("cfd-fill-damping", "Equivalent damping against fill", fill_labels,
                     [("Near predicted natural period; lower imposed roll", [val(c["measured"]) for c in chosen])], "Damping ratio; fill (%)", tick_precision=3))
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
    part.append(plot("cfd-loss-area", "Equivalent damping against conduit area", [f'{val(r["area"]):.1f}' for r in loss_checks],
                     [("CFD-derived, near predicted natural period", [val(c["measured"]) for c in near]),
                      ("Original inverse-area prediction", [val(c["linear_prediction"]) for c in near])], "Damping ratio; area (m²)", tick_precision=3))
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
    part.append(plot("cfd-forced-exchange", "Medium-mesh forced exchange", [f'{val(r["period"]):.0f}' for r in sweep],
                     [("Exchange amplitude", [val(r["exchange"]) for r in sweep])], "Exchange amplitude (m³); forcing period (s)"))
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
    # Counts are explicitly read from the review finding rather than inferred from generated inputs.
    screening = next(r for r in classes if val(r["result_family"]) == "screening manifest")
    rejected = next(r for r in classes if val(r["result_family"]) == "nine selected cases")
    part.append(plot("cfd-class-counts", "Analytical cases and failed wrappers: separate evidence classes", ["Analytical screening", "Rejected wrappers"],
                     [("Reviewed case counts; not completed CFD totals", [val(screening["case_count"]), val(rejected["case_count"])])], "Case count"))
    part.append(table(["Evidence class", "Result family", "Disposition", "Finding", "Limitation"],
                      [[v(r[key]) for key in ("evidence_class", "result_family", "status", "key_finding", "limitation")] for r in classes],
                      "The source-neutral review controls the evidence classification; methodology and validation are not interchangeable."))
    return "\n".join(part) + "\n</section>"


def render():
    d = compute()
    c = published_claims()
    p, geometry = d["inputs"], d["geometry"]
    style = SHOWCASE._CSS
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
    svg_period = chart("natural", "Rigid-column natural period", d["areas"],
                       [("Rigid-column model", d["natural"])], "Conduit area (m²)", "Period (s)")
    svg_inertia = chart("inertia", "Frozen-fluid roll inertia", d["areas"],
                        [("Frozen-fluid prismatic model", [v / 1e6 for v in d["inertia"]])],
                        "Conduit area (m²)", "Inertia (million kg m²)")
    svg_damping = chart("damping", "Quadratic-loss equivalent damping", d["areas"],
                        [(f"Level-difference amplitude {level:.1f} m", [v * 100 for v in ys])
                         for level, ys in zip(p["levels"], d["damping"])],
                        "Conduit area (m²)", "Damping ratio (%)")
    svg_moment = chart("moment", "Tank-on-vessel moment", d["periods"], [
        ("In phase", [m.in_phase / 1e6 for m in d["moments"]]),
        ("Quadrature", [m.quadrature / 1e6 for m in d["moments"]]),
        ("Magnitude", [m.magnitude / 1e6 for m in d["moments"]]),
    ], "Forcing period (s)", "Moment (MN m)", c["moment_min_period"])
    svg_roll = chart("roll", "Normalized coupled roll response", d["periods"],
                     list(zip(("Bare vessel", "Connected-tank model"), d["responses"])),
                     "Forcing period (s)", "Roll / unit wave moment", c["moment_min_period"])
    svg_split = chart("split", "Single-tank tuned-absorber benchmark", d["gs"],
                      [(f'Damping {a["zeta"]*100:.1f} %', a["ys"]) for a in d["absorbers"]],
                      "Forcing / vessel natural frequency", "Roll / static deflection")
    return f'''<!DOCTYPE html>
<html lang="en" data-theme="light"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Connected-tank sloshing analysis</title>
<link rel="stylesheet" href="../_assets/brand.css"><style>{style}</style></head>
<body><div class="wrap"><header><div class="kicker">Hydrodynamics · analytical study</div>
<h1>Connected-tank sloshing analysis</h1>
<p class="sub">Illustrative U-tube anti-roll response and a single-tank tuned-absorber comparison.</p></header>
<h2 id="summary">Summary</h2>
<p>An invented, illustrative U-tube system comprises two tank legs connected by a conduit. Analysis is
performed to determine the connected-tank response and compare it with a single-tank tuned absorber.
The geometry represents no particular vessel or client. The results are summarized below.</p>
<p>Analytical predictions are evaluated using the <a href="../../../src/digitalmodel/hydrodynamics/utube_tank.py">connected-tank model</a>
and the <a href="../../../scripts/cfd/reduce_tld_two_peak.py">tuned-absorber model</a>, with
<a href="#validation">published aggregate calibration results</a>. Archived computational fluid dynamics
(CFD) evidence is presented separately from the illustrative predictions. The predictions support screening; design qualification is not established.</p>
<p>The <a href="#limits">published moment-fit band</a> limits the use of the aggregate error statistic.
Transfer to the invented geometry remains an assumption, including inside that band. The
<a href="#comparison">single-tank comparison</a> illustrates peak splitting and merging; its amplitudes
do not establish a design ranking.</p>
<p>Draft for maintainer review.</p>
<h2 id="revisions">Revision history</h2>
<table><tr><th>Revision</th><th>Date</th><th>What changed and why</th></tr>
<tr><td>Display precision</td><td>2026-10-23</td><td>Damping-axis labels distinguished and comparison percentages rounded to one decimal place; source precision retained in the evidence record.</td></tr>
<tr><td>Evidence</td><td>2026-10-23</td><td>Archived CFD reductions added with provenance and evidence-class limits to distinguish measurements from illustrative predictions.</td></tr>
<tr><td>Geometry</td><td>2026-10-23</td><td>Independent illustrative dimensions and conduit area selected to separate the predictions from the reference geometry.</td></tr>
<tr><td>Editorial</td><td>Not assigned</td><td>Engineering descriptions clarified to distinguish illustrative predictions from calibration evidence.</td></tr>
<tr><td>Initial</td><td>Not assigned</td><td>Public-source analysis added to document model scope and remaining work.</td></tr></table>
<nav><a href="#geometry">Geometry</a> · <a href="#validation">Published validation</a> ·
<a href="#curves">Computed curves</a> · <a href="#comparison">Single-tank comparison</a> ·
<a href="#limits">Validity band</a> · <a href="#cfd-evidence">CFD evidence</a> · <a href="#forward">Way forward</a> · <a href="#left">Left</a></nav>
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
not established.</p>{svg_roll}
<h2 id="comparison">Single-tank comparison: split and merging</h2>
<p>The <a href="../../../scripts/cfd/reduce_tld_two_peak.py">tuned-absorber model</a> is evaluated at the
frequency grid and damping values in the <a href="../structural/sloshing-tld-twopeak.json">published response record</a>.
The mass ratio, denoted by μ, is {d["mu"]}; the frequency tuning ratio is {d["tuning"]:.1f}.
A single equivalent tank is coupled to an undamped primary vessel. The tank is not isolated, and its
geometry is not matched to the U-tube system. Different normalizations and hull damping are used in
the two models; their response amplitudes do not establish a design ranking.</p>
{svg_split}<table><tr><th>Tank damping (%)</th><th>Resolved peaks</th><th>Peak normalized roll</th></tr>{absorber_rows}</table>
<p>Two peaks are resolved in the light-damping curves; they merge as damping increases, and the peak
rises again at the highest damping shown in the table. These curves are evaluated using the
<a href="../../../scripts/cfd/reduce_tld_two_peak.py">single-tank model</a> and the
<a href="../structural/sloshing-tld-twopeak.json">published response record</a>. New connected-tank CFD
results are not included. The connected-tank curves are presented separately; a matched single-tank
and connected-tank comparison is not established.</p>
<h2 id="limits">Validity band</h2>
<p class="callout">The moment model degrades below about {c["degrades_below"]} s and the CAUSE IS NOT ESTABLISHED.
An earlier explanation was withdrawn. The residual also depends on roll amplitude.</p>
<p>The reported moment RMS applies at forcing periods of {c["moment_min_period"]} s or longer for the
<a href="../../../src/digitalmodel/hydrodynamics/utube_tank.py">published calibration</a>.
Intermediate periods are not qualified by that aggregate statistic. Even inside the band, transfer
to the invented geometry is an assumption. No new physical mechanism is proposed.</p>
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
Raw CFD fields, meshes, unstaged case quantities and the archived geometry are omitted. Continuous fill
coverage, matched single-tank versus connected-tank performance, and anti-roll phase or benefit remain
unestablished. The earlier campaign does not qualify the invented illustrative geometry.</p>
<h2 id="method">Analysis basis</h2>
<p>The analytical scope is described in the <a href="sloshing-cfd-study.html">sloshing study</a>.
The connected-tank calculations and aggregate calibration statistics are defined in the
<a href="../../../src/digitalmodel/hydrodynamics/utube_tank.py">analytical model</a> and
<a href="../../../tests/hydrodynamics/test_utube_tank.py">published calibration checks</a>.
The single-tank comparison is defined in the <a href="../../../scripts/cfd/reduce_tld_two_peak.py">tuned-absorber model</a>
and <a href="../structural/sloshing-tld-twopeak.json">response record</a>.</p>
<footer><a href="../../../scripts/capabilities/build_dual_tank_sloshing_study.py">Calculation basis for this study</a>.
Illustrative analytical predictions and separate archived CFD evidence; transfer remains unqualified.</footer></div></body></html>
'''


def build(output=OUTPUT):
    started = perf_counter()
    output.write_text(render(), encoding="utf-8", newline="\n")
    elapsed = perf_counter() - started
    print(f"Generated connected-tank study; wall time {elapsed:.3f} s")
    return elapsed


if __name__ == "__main__":
    build()
