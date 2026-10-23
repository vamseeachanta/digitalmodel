#!/usr/bin/env python
# ABOUTME: Build a source-neutral connected-tank analysis from public models.
"""Regenerate the connected-tank page using only committed analytical sources.

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
JSON_PATH = ROOT / "docs/api/structural/sloshing-tld-twopeak.json"
EXISTING = ROOT / "docs/api/cfd/sloshing-cfd-study.html"
OUTPUT = EXISTING.with_name("dual-tank-sloshing-cfd-study.html")


def load_module(name, path):
    spec = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(spec)
    sys.modules[name] = module
    spec.loader.exec_module(module)
    return module


MODEL = load_module("_dual_tank_public_model", MODEL_PATH)
REDUCTION = load_module("_dual_tank_public_reduction", REDUCTION_PATH)


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
    constants = inputs["constants"]
    manifest = json.loads(JSON_PATH.read_text(encoding="utf-8"))
    claims = published_claims()
    # Illustrative recombination of round public inputs, not the reference tank.
    geometry = MODEL.UTubeGeometry(
        leg_length=constants["LEG_LENGTH"],
        leg_width=constants["CENTROID_OFFSET"],
        fill_depth=constants["FILL_DEPTH"],
        centroid_offset=constants["LEG_WIDTH"],
    )
    area = constants["FILL_DEPTH"]  # illustrative section area in square metres
    count = len(manifest["frequency_ratio_grid"])
    areas = grid(min(inputs["areas"]), max(inputs["areas"]), count)
    periods = grid(claims["degrades_below"], inputs["max_period"], count)
    natural = [MODEL.natural_period(
        leg_area=geometry.leg_area, conduit_area=a, fill_depth=geometry.fill_depth,
        effective_conduit_length=geometry.effective_conduit_length,
    ) for a in areas]
    inertia = [MODEL.DEFAULT_DENSITY * MODEL._prismatic_properties(geometry, a)[2]
               for a in areas]
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


def render():
    d = compute()
    c = published_claims()
    p, geometry = d["inputs"], d["geometry"]
    style = re.search(r"<style>(.*?)</style>", EXISTING.read_text(encoding="utf-8"), re.S)[1]
    claims = (
        ("Quadratic conduit loss: calibrated area exponent", c["area_exponent"]),
        ('Published fit cases (aggregate outputs only)', c["cases"]),
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
                       [("Library natural_period", d["natural"])], "Conduit area (m²)", "Period (s)")
    svg_inertia = chart("inertia", "Frozen-fluid roll inertia", d["areas"],
                        [("Library prismatic properties", [v / 1e6 for v in d["inertia"]])],
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
<p>This invented geometry illustrates a connected pair of tank legs. It has no connection to any vessel
or client. Curves are analytical predictions from the committed library and reduction, using published
aggregate validation outputs. No new CFD was run. This is a screening analysis, not a design qualification.</p>
<h2 id="revisions">Revision history</h2>
<table><tr><th>Revision</th><th>Date</th><th>What changed and why</th></tr>
<tr><td>Initial</td><td>Not assigned</td><td>Public-source analysis added to document model scope and remaining work.</td></tr></table>
<nav><a href="#geometry">Geometry</a> · <a href="#validation">Published validation</a> ·
<a href="#curves">Computed curves</a> · <a href="#comparison">Single-tank comparison</a> ·
<a href="#limits">Validity band</a> · <a href="#forward">Way forward</a> · <a href="#left">Left</a></nav>
<h2 id="geometry">Illustrative geometry</h2>
<p>Round dimensions are recombined from the public test input constants; they do not reproduce the
reference geometry. Fill depth is an input, with no claim of a validated fill percentage.</p>
<table><tr><th>Input</th><th>Illustrative value</th><th>Public input basis</th></tr>
<tr><td>Each leg length × width</td><td>{geometry.leg_length:.1f} × {geometry.leg_width:.1f} m</td><td>LEG_LENGTH, CENTROID_OFFSET</td></tr>
<tr><td>Each leg plan area</td><td>{geometry.leg_area:.1f} m²</td><td>UTubeGeometry.leg_area</td></tr>
<tr><td>Leg centroid offset from centreline</td><td>{geometry.centroid_offset:.1f} m</td><td>LEG_WIDTH</td></tr>
<tr><td>Fill depth</td><td>{geometry.fill_depth:.1f} m</td><td>FILL_DEPTH</td></tr>
<tr><td>Conduit area for period sweeps</td><td>{d["area"]:.1f} m²</td><td>Round value from FILL_DEPTH, assigned area units</td></tr>
<tr><td>Effective conduit length</td><td>{geometry.effective_conduit_length:.1f} m</td><td>Library calibrated default, illustrative transfer only</td></tr>
<tr><td>Conduit input range in public period tests</td><td>{min(d["areas"]):.1f}–{max(d["areas"]):.1f} m²</td><td>Parameter inputs only; individual CFD measurements are not reproduced</td></tr></table>
<p>The illustrative conduit lies inside that public input range. The calibrated effective length is
borrowed for illustration; a new geometry requires a measured natural period before predictive use.</p>
<h2 id="validation">Published validation</h2>
<p>Quoted from the <a href="../../../src/digitalmodel/hydrodynamics/utube_tank.py">model docstring and constants</a>
and <a href="../../../tests/hydrodynamics/test_utube_tank.py">public test fit outputs</a>.</p>
<table><tr><th>Published quantity</th><th>Value</th></tr>{claim_rows}</table>
<p>The loss comparison uses refits of each area law, not substitution of an exponent at a fixed
coefficient. Anchor effective conduit length to a measured natural period, never the forced-sweep peak:
that peak is loss-controlled and moves with roll amplitude.</p>
<h2 id="curves">Computed curves</h2>
<p>Every curve calls the library functions directly. The inertia curve uses the library's frozen-fluid
prismatic roll inertia; it is distinct from a validation error for rigid-column conduit inertia.</p>
{svg_period}{svg_inertia}
<p>Damping is evaluated at forcing period {p["damping_period"]:.1f} s with imposed level-difference amplitudes
from the public damping test. It is amplitude-dependent, not a fixed tank property.</p>{svg_damping}
<p>The moment calculation imposes {p["roll_degrees"]:.1f}° roll at the illustrative conduit area.</p>{svg_moment}
<p>Coupled calculation inputs from the public response test: vessel roll period {p["vessel_period"]:.1f} s,
hull damping {p["hull_damping"]*100:.1f} %, tank authority {p["authority"]*100:.1f} %.
Authority is a normalized stiffness scale, not a mass ratio. The library fixes its forcing reference
through the bare-roll amplitude; the returned response is roll per unit wave moment at that finite
reference. Quadratic loss prevents an amplitude-independent RAO. Shading marks periods outside the
published moment-fit band. No wave-spectrum or dimensional vessel calibration is implied.</p>{svg_roll}
<h2 id="comparison">Single-tank comparison: split and merging</h2>
<p>The <a href="../../../scripts/cfd/reduce_tld_two_peak.py">committed tuned-absorber reduction</a> is evaluated
on the frequency grid and damping ladder in <a href="../structural/sloshing-tld-twopeak.json">the published JSON</a>.
Mass ratio μ = {d["mu"]}, frequency tuning ratio = {d["tuning"]:.1f}. This is a single equivalent tank on an
undamped primary vessel, not an isolated tank and not a matched geometry comparison with the U-tube.
The models use different normalizations and hull damping; their amplitudes cannot be used to rank designs.</p>
{svg_split}<table><tr><th>Tank damping (%)</th><th>Resolved peaks</th><th>Peak normalized roll</th></tr>{absorber_rows}</table>
<p>The light-damping curves split into adverse peaks; increasing damping merges them, and the heaviest
case raises the peak again. The ladder comes from the published single-tank reduction, not from new
connected-tank CFD. The connected geometry curves above are shown separately because no matched
single-tank and connected-tank campaign is published here.</p>
<h2 id="limits">Validity band</h2>
<p class="callout">The moment model degrades below about {c["degrades_below"]} s and the CAUSE IS NOT ESTABLISHED.
An earlier explanation was withdrawn. The residual also depends on roll amplitude.</p>
<p>The reported moment RMS applies at forcing periods of {c["moment_min_period"]} s or longer for the
published calibration. Intermediate periods are not qualified by that aggregate statistic. Even inside
the band, transfer to the invented geometry is an assumption. No new physical mechanism is proposed.</p>
<h2 id="forward">Way forward</h2><ul class="fw">
<li><b>Not done:</b> Measure the new geometry's natural period and recalibrate effective conduit length.</li>
<li><b>Not done:</b> Validate moment and loss across roll amplitudes and the short-period band.</li>
<li><b>Not done:</b> Compare single and connected tanks with matched mass, tuning, hull damping and forcing.</li>
<li><b>Not done:</b> Resolve the residual cause before widening the empirical validity band.</li>
<li><b>Not done:</b> Check operating fill, freeboard, structural loads and dimensional vessel coupling.</li></ul>
<h2 id="left">Left</h2>
<p>A numerical inertia-error bound and a fill-percentage validation range are not stated in the specified public sources.
Those claims are omitted pending public evidence. New CFD fields, individual CFD measurement plots,
and matched single-tank versus connected-tank performance are also omitted.</p>
<h2 id="method">Method and reproducibility</h2>
<p><code>python scripts/capabilities/build_dual_tank_sloshing_study.py</code> regenerates this page and prints
elapsed wall time. The builder reads the existing <a href="sloshing-cfd-study.html">sloshing study</a>'s style
block and shared brand stylesheet. It reads test inputs without executing tests; fit claims are read
from the model and test sources. SVGs are inline, with no external scripts, network or binary assets.</p>
<footer>Generated by <a href="../../../scripts/capabilities/build_dual_tank_sloshing_study.py">the committed builder</a>.
Analytical predictions and published aggregate fit statistics only.</footer></div></body></html>
'''


def build(output=OUTPUT):
    started = perf_counter()
    output.write_text(render(), encoding="utf-8", newline="\n")
    elapsed = perf_counter() - started
    print(f"Generated connected-tank study; wall time {elapsed:.3f} s")
    return elapsed


if __name__ == "__main__":
    build()
