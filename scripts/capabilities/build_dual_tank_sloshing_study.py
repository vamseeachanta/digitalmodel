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
<a href="#validation">published aggregate calibration results</a>. No new computational fluid dynamics
(CFD) results are included. The predictions support screening; design qualification is not established.</p>
<p>The <a href="#limits">published moment-fit band</a> limits the use of the aggregate error statistic.
Transfer to the invented geometry remains an assumption, including inside that band. The
<a href="#comparison">single-tank comparison</a> illustrates peak splitting and merging; its amplitudes
do not establish a design ranking.</p>
<p>Draft for maintainer review.</p>
<h2 id="revisions">Revision history</h2>
<table><tr><th>Revision</th><th>Date</th><th>What changed and why</th></tr>
<tr><td>Editorial</td><td>Not assigned</td><td>Engineering descriptions clarified to distinguish illustrative predictions from calibration evidence.</td></tr>
<tr><td>Initial</td><td>Not assigned</td><td>Public-source analysis added to document model scope and remaining work.</td></tr></table>
<nav><a href="#geometry">Geometry</a> · <a href="#validation">Published validation</a> ·
<a href="#curves">Computed curves</a> · <a href="#comparison">Single-tank comparison</a> ·
<a href="#limits">Validity band</a> · <a href="#forward">Way forward</a> · <a href="#left">Left</a></nav>
<h2 id="geometry">Illustrative geometry</h2>
<p>Round dimensions are recombined from the <a href="../../../tests/hydrodynamics/test_utube_tank.py">published reference inputs</a>;
the reference geometry is not reproduced. Fill depth is an input; a validated fill-percentage range is
not established.</p>
<table><tr><th>Input</th><th>Illustrative value</th><th>Source and assumption</th></tr>
<tr><td>Each leg length × width</td><td>{geometry.leg_length:.1f} × {geometry.leg_width:.1f} m</td><td>Reference leg length and centroid offset, reassigned as illustrative dimensions</td></tr>
<tr><td>Each leg plan area</td><td>{geometry.leg_area:.1f} m²</td><td>Product of the illustrative leg length and width</td></tr>
<tr><td>Leg centroid offset from centreline</td><td>{geometry.centroid_offset:.1f} m</td><td>Reference leg width, reassigned as the illustrative offset</td></tr>
<tr><td>Fill depth</td><td>{geometry.fill_depth:.1f} m</td><td>Reference fill-depth input</td></tr>
<tr><td>Conduit area for period sweeps</td><td>{d["area"]:.1f} m²</td><td>Reference fill-depth value, assigned area units for illustration</td></tr>
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
<p>A numerical inertia-error bound and a fill-percentage validation range are not stated in the specified public sources.
Those claims are omitted pending public evidence. New CFD fields, individual CFD measurement plots,
and matched single-tank versus connected-tank performance are also omitted.</p>
<h2 id="method">Analysis basis</h2>
<p>The analytical scope is described in the <a href="sloshing-cfd-study.html">sloshing study</a>.
The connected-tank calculations and aggregate calibration statistics are defined in the
<a href="../../../src/digitalmodel/hydrodynamics/utube_tank.py">analytical model</a> and
<a href="../../../tests/hydrodynamics/test_utube_tank.py">published calibration checks</a>.
The single-tank comparison is defined in the <a href="../../../scripts/cfd/reduce_tld_two_peak.py">tuned-absorber model</a>
and <a href="../structural/sloshing-tld-twopeak.json">response record</a>.</p>
<footer><a href="../../../scripts/capabilities/build_dual_tank_sloshing_study.py">Calculation basis for this study</a>.
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
