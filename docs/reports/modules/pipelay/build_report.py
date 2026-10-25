# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Rebuild the standalone pipelay analysis from the public workflow examples.

Use an explicit external scratch directory for analytical summary outputs.
The report contains no solver results or qualified environmental envelope.
"""

from __future__ import annotations

import argparse
import hashlib
import html
import json
import math
import re
from decimal import Decimal, ROUND_CEILING, ROUND_HALF_UP
from pathlib import Path

import yaml

from digitalmodel.pipelay.workflow import router

ROOT = Path(__file__).resolve().parents[4]
REVISION = "2"
DATE = "2026-10-22"
EXAMPLES = {
    "jlay": "examples/workflows/jlay-pipelay/input.yml",
    "reel": "examples/workflows/reel-lay-pipelay/input.yml",
}
ESC = html.escape


def load_example(case):
    """Read the documented example without altering its engineering inputs."""
    return yaml.safe_load((ROOT / EXAMPLES[case]).read_text(encoding="utf-8"))


def calculate(scratch):
    """Call the public router and retain strict, path-free calculation records."""
    record = {}
    for case in EXAMPLES:
        cfg = load_example(case)
        inputs = {key: cfg["pipelay"][key] for key in ("pipe", "config", "operability")}
        cfg["pipelay"]["outputs"]["directory"] = str(Path(scratch).resolve() / case)
        result = router(cfg)["pipelay"]
        payload = json.loads(Path(result["summary_json"]).read_text(encoding="utf-8"))
        assert all(result[key] == value for key, value in payload.items())
        record[case] = {"inputs": inputs, "result": payload}
    record["source_digests"] = {
        name: hashlib.sha256((ROOT / name).read_text(encoding="utf-8").encode("utf-8")).hexdigest()
        for name in [*EXAMPLES.values(), "src/digitalmodel/pipelay/workflow.py",
                     "src/digitalmodel/pipelay/validation.py",
                     "src/digitalmodel/orcaflex/pipelay_analysis.py",
                     "src/digitalmodel/pipelay/docs/screening-basis.md"]
    }
    return record


def lookup(record, key):
    """Resolve a dotted numerical binding, including list indices."""
    value = record
    for part in key.split("."):
        value = value[int(part)] if isinstance(value, list) else value[part]
    return value


def format_value(value, scale=1.0, decimals=1, rounding="nearest"):
    """Round once for display; minimum radii round upward and thickness uses three decimals."""
    scaled = float(value) * scale
    if not math.isfinite(scaled):
        raise ValueError("Report quantities must be finite")
    if decimals == 1 and rounding != "up" and scaled and abs(scaled) < 0.05:
        return f"{scaled:.1e}".replace("-", "−")
    mode = ROUND_CEILING if rounding == "up" else ROUND_HALF_UP
    displayed = Decimal(str(scaled)).quantize(Decimal(1).scaleb(-decimals), rounding=mode)
    return f"{displayed:.{decimals}f}".replace("-", "−")


def number(record, key, scale=1.0):
    """Bind visible numerical text to an exact module output or source input."""
    value = lookup(record, key)
    if key == "reel.result.residual_radius_m":
        result = record["reel"]["result"]
        status = result.get("residual_radius_status")
        if value is None:
            if status != "unbounded_zero_curvature" or result.get("residual_curvature_1_per_m") != 0:
                raise ValueError("Inconsistent residual radius and zero-curvature status")
            return (f'<span data-value="{key}" data-status="{status}">unbounded</span>')
        if status != "finite" or value <= 0:
            raise ValueError("Inconsistent residual radius and finite status")
    decimals = 3 if key.endswith(("wall_thickness", "coating_thickness")) else 1
    rounding = "up" if key.endswith("min_stinger_radius_m") else "nearest"
    return (f'<span data-value="{key}" data-scale="{scale}" data-decimals="{decimals}" data-rounding="{rounding}">'
            f'{format_value(value, scale, decimals, rounding)}</span>')


def screening_status(record, case):
    """Require the returned verdict to agree with unrounded strain and criterion."""
    result = record[case]["result"]
    demand = result["sagbend"]["sagbend_bending_strain"] if case == "jlay" else result["reeling_bending_strain"]
    limit = result["allowable_sagbend_strain"] if case == "jlay" else result["allowable_reeling_strain"]
    expected = "pass" if demand <= limit else "fail"
    if result["screening_status"] != expected:
        raise ValueError("Returned screening status disagrees with the strain criterion")
    return expected


def stress_verdict(record):
    """Compare the unrounded elastic stress proxy with the supplied yield strength."""
    stress = record["jlay"]["result"]["sagbend"]["sagbend_bending_stress_MPa"] * 1e6
    smys = float(record["jlay"]["inputs"]["pipe"]["smys"])
    return "below" if stress < smys else "equals" if stress == smys else "exceeds"


def source(name):
    """Keep source locators in the reference subsection."""
    labels = {"basis": "Analytical basis", "jlay": "Public J-lay example",
              "reel": "Public reel-lay example", "tests": "Regression evidence",
              "code": "Implemented interface", "clearance": "Evidence register"}
    return f'<a href="#source-{name}">{labels[name]}</a>'


def table(headers, rows, caption):
    """Render real columns; cell markup is supplied by this trusted template."""
    return ('<table><thead><tr>' + ''.join(f'<th scope="col">{ESC(h)}</th>' for h in headers)
            + '</tr></thead><tbody>' + ''.join('<tr>' + ''.join(f'<td>{c}</td>' for c in row)
                                              + '</tr>' for row in rows) + '</tbody></table>'
            + '<p class="table-caption">' + ESC(caption) + '</p>')


def house_style():
    """Inline the canonical tokens and the existing prose-report stylesheet."""
    brand = (ROOT / "docs/api/_assets/brand.css").read_text(encoding="utf-8")
    exemplar = (ROOT / "docs/api/cfd/turbulent-flat-plate-verification.html").read_text(encoding="utf-8")
    prose = re.search(r"<style>(.*?)</style>", exemplar, re.S)[1]
    # CSS comments are omitted; all declarations are preserved without changes.
    declarations = re.sub(r"/\*.*?\*/", "", brand + prose, flags=re.S)
    return '<style>' + '\n'.join(line.rstrip() for line in declarations.splitlines()) + '</style>'


def changes():
    """Describe engineering content introduced at this issue."""
    return {"revision": REVISION, "date": DATE, "changes": [
        "Screening wording and chart colours follow calculated strain and stress comparisons.",
        "Unrounded workflow results are formatted once; minimum radii are rounded upward.",
        "UTF-8 and LF-normalised source and report digests retain portable comment bindings.",
        "Independent numerical anchors, captions and total reeling-strain terminology are retained.",
    ]}


def schematic(record):
    """Draw the actual catenary at equal horizontal and vertical length scales."""
    r = record["jlay"]["result"]
    a = r["sagbend"]["catenary_parameter_m"]
    depth = r["water_depth_m"]
    offset = a * math.acosh(1 + depth / a)
    scale = 430 / depth
    points = ' '.join(f'{340 + offset * i / 80 * scale:.3f},'
                      f'{520 - a * (math.cosh(offset * i / 80 / a) - 1) * scale:.3f}'
                      for i in range(81))
    pipe = record["jlay"]["inputs"]["pipe"]
    coating_radius = 68
    steel_radius = coating_radius * pipe["outer_diameter"] / (pipe["outer_diameter"] + 2 * pipe["coating_thickness"])
    bore_radius = steel_radius * (pipe["outer_diameter"] - 2 * pipe["wall_thickness"]) / pipe["outer_diameter"]
    def svg_number(key, scale=1):
        return number(record, key, scale).replace('<span', '<tspan').replace('</span>', '</tspan>')

    return f'''<figure><svg viewBox="0 0 820 600" role="img" aria-labelledby="geometry-title geometry-desc">
<title id="geometry-title">J-lay geometry and coated pipe section</title>
<desc id="geometry-desc">A static pipe hangs from a fixed top point to horizontal touchdown on a flat seabed.
The line coordinates use equal length scales. The annulus inset uses the example diameter ratios.</desc>
<defs><marker id="arrow" markerWidth="8" markerHeight="8" refX="7" refY="4" orient="auto-start-reverse"><path d="M0,0 L8,4 L0,8" fill="var(--ink)"/></marker></defs>
<path d="M40 90 H770" stroke="var(--brand)" fill="none"/>
<text x="50" y="75">Still water surface; fixed top point</text>
<path d="M40 520 H770 L770 550 H40 Z" fill="var(--soft)" stroke="var(--mut)"/>
<text x="50" y="575">Flat seabed; horizontal tangent; no soil restraint</text>
<path d="M95 520 H340" stroke="var(--ink)" stroke-width="4"/>
<polyline points="{points}" fill="none" stroke="var(--brand)" stroke-width="4"/>
<path d="M{340 + offset * scale:.3f} 90 v-48" stroke="var(--ink)" marker-end="url(#arrow)"/>
<text x="445" y="48">Top force T; angle θ from vertical</text>
<path d="M265 90 V520" stroke="var(--ink)" marker-start="url(#arrow)" marker-end="url(#arrow)"/>
<text x="60" y="300">Depth h (m): {svg_number('jlay.result.water_depth_m')}</text>
<path d="M340 548 H{340 + offset * scale:.3f}" stroke="var(--ink)" marker-start="url(#arrow)" marker-end="url(#arrow)"/>
<text x="445" y="500">Touchdown offset x (m): {svg_number('jlay.result.sagbend.horizontal_offset_m')}</text>
<text x="445" y="525">Pipe weight w acts downward</text>
<circle cx="630" cy="235" r="68" fill="var(--soft)" stroke="var(--mut)"/>
<circle cx="630" cy="235" r="{steel_radius:.3f}" fill="var(--mut)"/>
<circle cx="630" cy="235" r="{bore_radius:.3f}" fill="var(--panel)"/>
<text x="535" y="340">Coating / steel / empty bore</text>
<text x="505" y="365">Steel diameter (mm): {svg_number('jlay.inputs.pipe.outer_diameter', 1000)}</text>
<text x="505" y="390">Wall thickness (mm): {svg_number('jlay.inputs.pipe.wall_thickness', 1000)}</text>
<text x="505" y="415">Radial coating (mm): {svg_number('jlay.inputs.pipe.coating_thickness', 1000)}</text>
</svg><figcaption>Figure C. The catenary shape is computed from the example geometry.
The cross-section is enlarged independently. Pipe line thickness is symbolic.
No support-vessel geometry, crane or rigging arrangement is represented. {source('basis')}; {source('jlay')}.</figcaption></figure>'''


def rate_chart(record, case):
    """Render module rate samples and the supplied cutoff without interpolation."""
    op = record[case]["result"]["operability"]
    rows = op["lay_rate_table"]
    xmax = max(row["hs_m"] for row in rows)
    ymax = max(row["lay_rate_km_per_day"] for row in rows)
    limit = op["operating_limit_m"]
    name = "J-lay" if case == "jlay" else "Reel-lay"
    figure = "B" if case == "jlay" else "D"
    x = lambda value: 75 + value / xmax * 620
    y = lambda value: 300 - value / ymax * 225
    points = []
    for row in rows:
        hs, rate = row["hs_m"], row["lay_rate_km_per_day"]
        text = (f'{name}; joining and laying' if case == 'jlay' else f'{name}; laying')
        text += (f'; significant wave height {format_value(hs)} m; rate {format_value(rate)} km/day; '
                 f'wave-height cutoff {format_value(limit)} m; supplied assumption')
        color = "var(--go)" if hs <= limit else "var(--nogo)"
        points.append(f'<circle cx="{x(hs):.3f}" cy="{y(rate):.3f}" r="6" fill="{color}" '
                      f'tabindex="0" data-point="{case}:{hs}" aria-label="{ESC(text)}"><title>{ESC(text)}</title></circle>')
    return f'''<figure><svg viewBox="0 0 780 365" role="img" aria-label="{name} assumed lay-rate screen" data-limit="{limit}">
<path d="M75 65 V300 H705" fill="none" stroke="var(--ink)"/>
<path class="limit" d="M{x(limit):.3f} 65 V300" stroke="var(--caution)" stroke-dasharray="6 4"><title>Supplied wave-height cutoff: {format_value(limit)} m</title></path>
<text x="85" y="40">{name}: assumed rate (km/day)</text>
<text x="300" y="355">Significant wave height (m)</text>
<text x="45" y="305">{format_value(0)}</text><text x="35" y="80">{format_value(ymax)}</text>
<text x="70" y="325">{format_value(0)}</text><text x="680" y="325">{format_value(xmax)}</text>
{''.join(points)}</svg><figcaption>Figure {figure}. {name} rate samples. Green: the supplied wave-height condition is met.
Red: the supplied condition is exceeded. Amber: the supplied cutoff is shown.
Hover or focus a point for the case, component, rate, cutoff and units.
These colours describe assumed conditions, not installation acceptance. {source(case)}; {source('basis')}.</figcaption></figure>'''


def component_chart(record):
    """Show each strain demand as a percentage of its own assumed limit."""
    parts = [("J-lay sagbend", 'jlay.result.sagbend.sagbend_bending_strain',
              'jlay.result.allowable_sagbend_strain'),
             ("Reel core", 'reel.result.reeling_bending_strain', 'reel.result.allowable_reeling_strain')]
    bars = []
    for i, (label, demand_key, limit_key) in enumerate(parts):
        demand, limit = lookup(record, demand_key), lookup(record, limit_key)
        # Utilisation remains unrounded until its engineering display is rendered.
        key = ('jlay.result.sagbend_strain_utilisation' if i == 0
               else 'reel.result.reeling_strain_utilisation')
        percent = lookup(record, key) * 100
        status = screening_status(record, "jlay" if i == 0 else "reel")
        verdict = "passes" if status == "pass" else "fails"
        color = "var(--go)" if status == "pass" else "var(--nogo)"
        tip = (f'{label}; demand {format_value(demand, 1e6)} µm/m; assumed limit '
               f'{format_value(limit, 1e6)} µm/m; utilisation {format_value(percent)} %; {verdict}')
        yy = 75 + i * 65
        bars.append(f'<text x="20" y="{yy + 20}">{label}</text><rect x="215" y="{yy}" '
                    f'width="{percent * 3.6:.3f}" height="30" fill="{color}" tabindex="0" '
                    f'aria-label="{ESC(tip)}"><title>{ESC(tip)}</title></rect>')
    return ('<figure><svg viewBox="0 0 780 255" role="img" aria-label="Strain utilisation by screening component">' +
        '<text x="215" y="35">Strain demand / assumed strain limit (%)</text>' + \
        '<path class="limit" d="M575 50 V190" stroke="var(--caution)" stroke-dasharray="6 4"/>' + \
        ''.join(bars) + '<text x="200" y="220">0.0</text><text x="555" y="220">100.0</text></svg>' + \
        f'<figcaption>Figure A. Green bars pass and red bars fail their own assumed strain criteria. '
        f'The amber line marks equality to that criterion. Only the listed strain components are assessed. '
        f'Hover or focus a bar for its demand and limit. {source("basis")}.</figcaption></figure>')


def input_table(record):
    """Inventory the exact source inputs with unit conversions for readable precision."""
    pipe_fields = [("outer_diameter", "Steel outside diameter", "mm", 1000),
                   ("wall_thickness", "Steel wall thickness", "mm", 1000),
                   ("coating_thickness", "Radial coating thickness", "mm", 1000),
                   ("steel_density", "Steel density", "kg/m³", 1),
                   ("coating_density", "Coating density", "kg/m³", 1),
                   ("seawater_density", "Seawater density", "kg/m³", 1),
                   ("youngs_modulus", "Young’s modulus", "GPa", 1e-9),
                   ("smys", "Specified minimum yield strength", "MPa", 1e-6)]
    rows = [[label, number(record, f'jlay.inputs.pipe.{key}', scale),
             number(record, f'reel.inputs.pipe.{key}', scale), unit,
             source('jlay') + '; ' + source('reel')] for key, label, unit, scale in pipe_fields]
    return table(["Quantity", "J-lay", "Reel-lay", "Unit", "Source"], rows,
                 "Table 2. Illustrative pipe inputs and their source examples.")


def rate_input_table(record):
    """Retain all empirical rate coefficients and cutoffs in the data register."""
    fields = [("max_hs_lay", "Laying wave-height cutoff", "m"),
              ("max_hs_welding", "Joining wave-height cutoff", "m"),
              ("max_hs_aband_recovery", "Abandonment and recovery cutoff", "m"),
              ("lay_rate_calm", "Calm-water lay rate", "km/day"),
              ("lay_rate_degradation", "Rate degradation per unit wave height", "km/day/m")]
    return table(["Quantity", "J-lay", "Reel-lay", "Unit", "Source"], [
        [label, number(record, f'jlay.inputs.operability.{key}'),
         number(record, f'reel.inputs.operability.{key}'), unit,
         source('jlay') + '; ' + source('reel')] for key, label, unit in fields],
         "Table 3. Assumed lay-rate inputs and wave-height cutoffs.")


def result_table(record, case, fields):
    """Show source-bound quantities, rounding once for engineering display."""
    return table(["Quantity", "Value", "Unit", "Source"], [
        [label, number(record, f'{case}.result.{key}', scale), unit,
         source(case) + '; ' + source('basis')]
        for key, label, unit, scale in fields],
        "Table 6. J-lay analytical results." if case == "jlay" else "Table 7. Reel-lay analytical results.")


def content(record):
    """Write the complete engineering narrative with the module scope in context."""
    n = lambda key, scale=1: number(record, key, scale)
    jpass = screening_status(record, "jlay") == "pass"
    rpass = screening_status(record, "reel") == "pass"
    jcomparison = "meets" if jpass else "exceeds"
    rcomparison = "meets" if rpass else "exceeds"
    jverdict = "passes" if jpass else "fails"
    rverdict = "passes" if rpass else "fails"
    stress = stress_verdict(record)
    stress_comparison = "is below" if stress == "below" else stress
    overall = ("The public examples meet the selected strain screens under the stated assumptions."
               if jpass and rpass else
               "At least one public example exceeds its selected strain criterion; revise the inputs and reassess.")
    jfields = [
        ('water_depth_m', 'Water depth', 'm', 1),
        ('tower_angle_deg', 'Top angle from vertical', 'degrees', 1),
        ('supplied_top_tension_kN', 'Supplied tension for consistency check', 'kN', 1),
        ('top_tension_kN', 'Derived top tension', 'kN', 1),
        ('pipe_submerged_weight_N_per_m', 'Empty-pipe submerged weight', 'N/m', 1),
        ('sagbend.horizontal_tension_kN', 'Horizontal tension', 'kN', 1),
        ('sagbend.sagbend_radius_m', 'Touchdown radius', 'm', 1),
        ('sagbend.sagbend_bending_strain', 'Touchdown bending strain', 'µm/m', 1e6),
        ('allowable_sagbend_strain', 'Assumed sagbend strain limit', 'µm/m', 1e6),
        ('sagbend_strain_utilisation', 'Sagbend strain utilisation', '%', 100),
        ('sagbend.sagbend_bending_stress_MPa', 'Elastic bending stress proxy', 'MPa', 1),
        ('sagbend.horizontal_offset_m', 'Touchdown horizontal offset', 'm', 1),
        ('sagbend.suspended_length_m', 'Suspended length', 'm', 1),
        ('min_stinger_radius_m', 'Reference overbend radius', 'm', 1),
    ]
    rfields = [
        ('reel_core_radius_m', 'Reel core radius', 'm', 1),
        ('reeling_bending_strain', 'Core bending strain', 'µm/m', 1e6),
        ('allowable_reeling_strain', 'Assumed reeling strain limit', 'µm/m', 1e6),
        ('reeling_strain_utilisation', 'Reeling strain utilisation', '%', 100),
        ('straightener_efficiency', 'Assumed curvature removal', '%', 100),
        ('residual_curvature_1_per_m', 'Residual curvature', '1/km', 1000),
        ('residual_radius_m', 'Residual radius', 'm', 1),
        ('residual_bending_strain', 'Residual bending strain', 'µm/m', 1e6),
        ('residual_out_of_straightness_span_m', 'Assessment span', 'm', 1),
        ('residual_out_of_straightness_m', 'Small-slope residual span offset', 'mm', 1000),
    ]
    return f'''
<section id="executive-summary"><h2>1. Executive Summary</h2>
<p>Pipelay is an analytical pipeline installation screening module. Analysis is performed to determine
static suspended geometry, bending strain and assumed lay-rate restrictions; key results are summarized below.</p>
<ul><li>The public J-lay example {jcomparison} the assumed sagbend strain limit.
<ul><li>Strain utilisation is {n('jlay.result.sagbend_strain_utilisation', 100)} % against
the assumed limit of {n('jlay.result.allowable_sagbend_strain', 1e6)} µm/m.
The example is suitable for reproducing the static screen, not for authorising installation.</li></ul></li>
<li>The public reel-lay example {rcomparison} the assumed core strain limit.
<ul><li>Strain utilisation is {n('reel.result.reeling_strain_utilisation', 100)} % against
the assumed limit of {n('reel.result.allowable_reeling_strain', 1e6)} µm/m.
The straightener response remains an input assumption.</li></ul></li></ul>
<p>Vessel orientation, station-keeping tolerance and an installation sea-state envelope are NOT_EVALUATED.
The assumed joining cutoff of {n('jlay.result.operability.operating_limit_m')} m significant wave height
does not establish a safe operating limit. {source('jlay')}; {source('reel')}; {source('basis')}.</p>
<h3 id="revision-history">Revision history</h3>
{table(['Revision', 'Date', 'What changed and why'], [
['1', '2026-10-21', 'Initial analytical module analysis; example calculations and evidence limits are presented for engineering use.'],
[REVISION, DATE, 'Corrected calculated verdicts, display precision and portable reproduction; added independent anchors and captions.']],
'Table 1. Report revision history.')}
</section>

<section id="introduction"><h2>2. Introduction</h2>
<p>The supported workflow is defined as static J-lay and reel-lay screening. The intended use is preliminary
input checking and transparent reproduction of analytical quantities before a separately qualified installation analysis.</p>
<p>The public router accepts a configuration mapping, selects the lay method, rejects invalid design inputs,
writes a strict JavaScript Object Notation (JSON) summary and returns the augmented mapping.
The direct calculation functions return result mappings without writing files. {source('code')}.</p>
<p>J-lay geometry is solved using depth and the angle from vertical. Supplied top tension is a consistency check,
not an independent means of tuning curvature at a fixed depth and angle. Reel-lay screening uses the innermost
wrap radius and a prescribed fraction of curvature removal. {source('basis')}.</p>
</section>

<section id="summary"><h2>3. Summary</h2>
<p>The detailed screening controls are presented as strain utilisation and assumed wave-height cutoffs.
A combined installation envelope is not established because period, heading and dynamic component demands are absent.</p>
{component_chart(record)}
{rate_chart(record, 'jlay')}
<p>The J-lay rate is controlled by the lower of the supplied laying and joining wave-height limits.
The cutoff is {n('jlay.result.operability.operating_limit_m')} m; equality is permitted by the rate rule.
Abandonment and recovery capacity is not assessed by that rule. {source('basis')}; {source('jlay')}.</p>
<p>A live forecast, wave-period envelope, orientation tolerance and forecast-support trigger are NOT_EVALUATED.
Operating instructions require a qualified vessel response model and approved criteria before a forecast service
or heading restriction is specified. {source('clearance')}.</p>
</section>

<section id="design-data"><h2>4. Design Data</h2>
<p>The design basis is taken from the repository’s public illustrative inputs. Pipe contents are excluded,
and all material values and selected limits remain ASSUMED - to be confirmed for an installation.</p>
<h3>4.1 Physical system and inputs</h3>
{schematic(record)}
<p>Lengths are supplied in metres, force in kilonewtons, density in kilograms per cubic metre and strain as a
dimensionless fraction. Report strains are expressed in micrometres per metre. The complete pipe data register
is retained in the appendix. Vessel, crane, rigging and response-amplitude data are not supplied. {source('jlay')}; {source('reel')}.</p>
<h3>4.2 Criteria and rationale</h3>
<p>The strain screen compares computed bending strain with the explicitly supplied example limit. The reference
overbend radius uses the supplied strain limit of {n('jlay.inputs.config.allowable_overbend_strain', 1e6)} µm/m;
it is a geometric reference, not a model of a fitted stinger. {source('jlay')}; {source('basis')}.</p>
<p>J-lay tension closure is checked using the returned relative tolerance of
{n('jlay.result.sagbend.tension_closure_relative_tolerance', 100)} %. This input-consistency check prevents a
depth, angle and force combination that violates the assumed static equilibrium. Finite positive submerged weight
and a nonzero near-vertical angle are required. Input plausibility bounds detect unit mistakes; they are not design allowables.
{source('basis')}.</p>
<p>No standard-derived acceptance limit is applied. The implementation’s inherited standards references lack
qualified editions and clauses for this use; standards compliance is NOT_EVALUATED. {source('clearance')}.</p>
</section>

<section id="methodology"><h2>5. Methodology</h2>
<p>The geometry is derived from static equilibrium of an inextensible flexible line with uniform submerged
weight and a horizontal touchdown tangent. Bending strain is then estimated from the catenary curvature.</p>
<p>Let D be steel outside diameter, t steel wall thickness, c radial coating thickness, and Dᵢ and D꜀ the bore
and coated diameters. Areas Aₛ, A꜀ and A𝑑 denote steel, coating and displaced cross-section. Densities ρₛ, ρ꜀ and
ρ𝑤 denote steel, coating and water; g is the gravitational acceleration convention retained by the implementation.</p>
<div class="eq">Dᵢ = D − 2t; D꜀ = D + 2c; Aₛ = π(D² − Dᵢ²)/4;<br>
A꜀ = π(D꜀² − D²)/4; A𝑑 = πD꜀²/4; w = g(ρₛAₛ + ρ꜀A꜀ − ρ𝑤A𝑑).</div>
<p>The annular areas and empty-pipe buoyancy expression are defined by the analytical basis. Its gravity
convention is retained without a new environmental assumption. {source('basis')}.</p>
<p>Let h be water depth, θ top angle from vertical, T top resultant force, H horizontal force, a catenary parameter,
x horizontal touchdown offset, z height above seabed and s suspended arc length. Consistent force units are used.</p>
<div class="eq">H = T sin θ; T = H + wh; a = H/w = h sin θ/(1 − sin θ);<br>
z = a[cosh(x/a) − 1]; x = a acosh(1 + h/a); s = √(h² + 2ah).</div>
<p>Let κ be touchdown curvature, ε bending strain, E Young’s modulus and σ the elastic stress proxy.
The minimum reference radius Rₘᵢₙ follows from an assumed maximum strain εₘₐₓ.</p>
<div class="eq">κ = 1/a; ε = Dκ/2; σ = Eε; Rₘᵢₙ = D/(2εₘₐₓ).</div>
<p>These equations and their numerical constants are geometric identities derived in the analytical basis,
not values copied from a licensed standard. Axial extension, flexural stiffness in equilibrium, contents pressure,
contact, hydrodynamic forcing and material plasticity are excluded. The elastic proxy is not a combined stress check.
{source('basis')}.</p>
<p>For reel-lay, let R be core radius, η the assumed fraction of curvature removed, κᵣ residual curvature,
L the measurement span and δ its small-slope midspan offset.</p>
<div class="eq">εᵣₑₑₗ = D/(2R); κᵣ = (1 − η)/R; εᵣ = Dκᵣ/2; δ ≈ κᵣL²/8.</div>
<p>The straightener fraction is prescribed rather than predicted from cyclic material response. Perfect removal
produces zero curvature and an unbounded residual radius encoded as null in JSON. {source('basis')}.</p>
<p>For the assumed rate rule, Hₛ denotes significant wave height, v₀ calm-water rate, b degradation per unit
wave height and Hₛ,ₗᵢₘ the supplied cutoff. The rate is zero above the cutoff; otherwise v = max(0, v₀ − bHₛ).
J-lay uses the lower laying and joining cutoff; reel-lay uses the laying cutoff. {source('basis')}.</p>
</section>

<section id="detailed-results"><h2>6. Detailed Results</h2>
<p>The example results are obtained by calling the public router on the documented input files.
Only the output destination is redirected; engineering inputs are retained.</p>
<section id="worked-example"><h3>J-lay worked example</h3>
<p>The calculated radius and top tension follow from the supplied depth and angle. The detailed values are
retained in the appendix, rounded once in engineering display units; minimum reference radii are rounded upward.</p>
<p>At depth {n('jlay.result.water_depth_m')} m and top angle {n('jlay.result.tower_angle_deg')} degrees from vertical,
the top tension is {n('jlay.result.top_tension_kN')} kN and the touchdown radius is
{n('jlay.result.sagbend.sagbend_radius_m')} m. Touchdown offset is
{n('jlay.result.sagbend.horizontal_offset_m')} m and suspended length is
{n('jlay.result.sagbend.suspended_length_m')} m. {source('jlay')}; {source('basis')}.</p>
<p>Bending strain is {n('jlay.result.sagbend.sagbend_bending_strain', 1e6)} µm/m against the assumed limit of
{n('jlay.result.allowable_sagbend_strain', 1e6)} µm/m; utilisation is
{n('jlay.result.sagbend_strain_utilisation', 100)} %. The elastic bending stress proxy is
{n('jlay.result.sagbend.sagbend_bending_stress_MPa')} MPa. {source('jlay')}; {source('basis')}.</p>
<p>The sagbend strain screen {jverdict} against the assumed limit. The supplied tension closes within the
input-consistency tolerance. The elastic stress proxy {stress_comparison} the example’s specified minimum yield strength of
{n('jlay.inputs.pipe.smys', 1e-6)} MPa; this observation does not assess axial stress, local buckling or installation acceptance.
{source('basis')}; {source('jlay')}.</p></section>
<h3>Reel-lay example</h3>
<p>The core strain is {n('reel.result.reeling_bending_strain', 1e6)} µm/m against the assumed limit of
{n('reel.result.allowable_reeling_strain', 1e6)} µm/m. The residual span offset is
{n('reel.result.residual_out_of_straightness_m', 1000)} mm over the supplied span of
{n('reel.result.residual_out_of_straightness_span_m')} m. The full component values are retained in the appendix.
{source('reel')}; {source('basis')}.</p>
<p>The core strain screen {rverdict} against the assumed total reeling-strain criterion. The residual offset is calculated
using the supplied straightener fraction and span; no residual-offset acceptance criterion is supplied, so its
engineering acceptance is NOT_EVALUATED. {source('basis')}; {source('reel')}.</p>
{rate_chart(record, 'reel')}
</section>

<section id="validation"><h2>7. Validation</h2>
<p>Repository evidence is provided by independent arithmetic anchors and analytical regression assertions.
These records check implementation behaviour; field validation and native installation qualification are NOT_EVALUATED.</p>
<p>The documented anchors cover submerged weight, catenary parameter, top tension, strain, offset and suspended
length. Reel checks cover core strain, residual curvature, radius, strain and span offset. The anchor comparison
uses the analytical basis’s relative tolerance, equal to the tension-closure tolerance displayed in the design basis.
{source('basis')}; {source('tests')}.</p>
<p>Boundary tests reject unsupported methods, missing inputs, unknown keys, impossible annuli, non-finite values,
non-positive submerged weight and inconsistent force closure before summary output. Regression assertions also
cover perfect straightening, unrounded strain acceptance, the joining cutoff and strict summary readback.
{source('tests')}.</p>
<p>The report rebuild test checks displayed values and units against independent hand-worked literals and
compares all numerical bindings with fresh router returns. It also checks that
worked-example numerical text has a binding and compares the committed HTML with a deterministic rebuild.
These checks establish reproducibility of the presentation, not independent physical qualification. {source('tests')}.</p>
</section>

<section id="recommendations"><h2>8. Key Findings, Limitations and Recommendations</h2>
<p>{overall} Engineering release
is restricted by missing operating conditions, acceptance criteria and independent installation evidence.</p>
<p>Use the J-lay calculation to check static input consistency and preliminary curvature. Confirm pipe contents,
coating tolerances, material certificates and the governing edition-qualified criteria before extending the screen
to an installation assessment. Obtain a qualified response analysis for vessel motion, waves, current, axial load,
touchdown contact, local buckling and fatigue. The evidence register identifies these installation-evidence gaps.
{source('clearance')}.</p>
<p>For an exceeded J-lay strain criterion, increase the touchdown radius by selecting a consistent depth, angle
and force combination, then reassess the full installation model. For an exceeded reel criterion, increase the
core radius or revise the pipe selection against qualified material limits. These actions follow the inverse
radius relation for strain; a passing screen alone does not release the installation. {source('basis')}.</p>
<p>Obtain measured or qualified straightener response and an approved residual-offset criterion before judging
reel-lay residual curvature. Retain the rate cutoffs as planning assumptions until joined-up vessel and joining
constraints are demonstrated; no forecast-support sea state is assigned. {source('basis')}; {source('clearance')}.</p>
<h3>Not covered</h3>
<p>Supporting S-lay stress estimates and departure-angle tables, native model generation, dynamic response,
diffraction, fatigue, cyclic plasticity, ovalisation, abandonment and recovery, station keeping and forecast services
are outside this analytical workflow report. The supporting S-lay depth-table regression retains an expected failure
because its angle expression is independent of depth. Restrict that table to non-design inspection until a
depth-dependent formulation and independent reference cases are provided. {source('tests')}; {source('clearance')}.</p>
</section>

<section id="references"><h2>9. References</h2>
<p>The source basis is limited to public repository records available with this report. No external published
benchmark, licensed standard value or field dataset is substituted for the repository examples.</p>
<h3>9.1 Engineering basis</h3>
<p>The analytical screening basis, as identified by the knowledge pack dated 2026-10-18, supplies equilibrium,
geometry, curvature and rate assumptions. The public examples supply all applied numerical inputs and strain
criteria. The knowledge pack and evidence register distinguish analytical regression from installation qualification.</p>
<h3>9.2 Source and reproducibility locators</h3>
<ul class="refs">
<li id="source-basis"><a href="../../../../src/digitalmodel/pipelay/docs/screening-basis.md">Analytical screening basis</a>:
equations E1–E7, validation criteria V1–V4 and independently evaluated anchors;
<a href="../../../../src/digitalmodel/pipelay/docs/agent-pack.md">knowledge pack</a>, revision 1, dated 2026-10-18, sections 1–8.</li>
<li id="source-jlay"><a href="../../../../examples/workflows/jlay-pipelay/input.yml">Public J-lay input</a>:
pipe, config and operability mappings; illustrative data, not a published field benchmark.</li>
<li id="source-reel"><a href="../../../../examples/workflows/reel-lay-pipelay/input.yml">Public reel-lay input</a>:
pipe, config and operability mappings; illustrative data, not qualified cyclic material data.</li>
<li id="source-code"><a href="../../../../src/digitalmodel/pipelay/workflow.py">Workflow interface</a>:
router, run_jlay, run_reel_lay and summary writer;
<a href="../../../../src/digitalmodel/pipelay/validation.py">input validation</a>;
<a href="../../../../src/digitalmodel/orcaflex/pipelay_analysis.py">analytical classes</a>:
PipelayPipeProperties, JLayConfig, LayOperability and calculate_stinger_radius.</li>
<li id="source-tests"><a href="../../../../tests/pipelay/test_pipelay_workflow.py">Workflow tests</a>;
<a href="../../../../tests/pipelay/test_pack_gates.py">analytical gate tests</a>;
<a href="../../../../tests/pipelay/test_audit_regressions.py">boundary and independent-anchor tests</a>;
<a href="../../../../tests/pipelay/test_review_clearance_index.py">evidence-register tests</a>;
<a href="../../../../tests/orcaflex/test_pipelay_analysis.py">supporting analytical tests</a>, including the
strict expected failure in test_departure_angle_recomputed_with_depth;
<a href="../../../../tests/pipelay/test_module_report.py">report rebuild tests</a>.</li>
<li id="source-clearance"><a href="../../../registry/pipelay-review-clearance.json">Review and clearance register</a>:
pack and installation evidence requirements; execution fields are not execution receipts.</li>
<li><a href="build_report.py">Report generator</a>: with the repository source on the Python import path,
execute <code>python docs/reports/modules/pipelay/build_report.py --output-dir docs/reports/modules/pipelay --scratch-dir "$TMPDIR/pipelay-report"</code>.
Dependencies: NumPy, pandas, SciPy, Pydantic and PyYAML; pytest and pytest-asyncio are used for the rebuild tests.</li>
<li>Presentation exemplar: <a href="../../../api/cfd/turbulent-flat-plate-verification.html">existing CFD prose report</a>;
canonical stylesheet: <a href="../../../api/_assets/brand.css">brand tokens</a>. Their style declarations are inlined.</li>
</ul>
</section>

<section id="appendices"><h2>10. Appendices</h2>
<p>Input assumptions, interface coverage and presentation checks are retained for engineering review.
Comments are saved separately so that the calculation and report remain unchanged.</p>
<h3>10.1 Data register</h3>
{input_table(record)}
{rate_input_table(record)}
<p>All values in the input tables and example configuration are ASSUMED - to be confirmed.
The source is the public illustrative example, not a material certificate or an earlier installation.</p>
{table(['Item', 'Treatment', 'Evidence / action'], [
['Depth, angle and force', 'ASSUMED - to be confirmed', 'J-lay example configuration; confirm survey and installation geometry.'],
['Strain limits and curvature removal', 'ASSUMED - to be confirmed', 'Example configuration; confirm material qualification and governing criteria.'],
['Calm rate, degradation and sea-state cutoffs', 'ASSUMED - to be confirmed', 'Example operability mapping; establish response-based operating limits.'],
['Optional pipe property', 'Poisson’s ratio uses the module default; unused in these equations.', 'Applied defaults are retained in the embedded calculation record; no additional material inference is made.'],
['Contents, pressure, vessel response and forecast', 'NOT_EVALUATED', 'Required inputs and qualified analysis are absent.'],
['Residual-offset acceptance', 'NOT_EVALUATED', 'An approved criterion and straightener qualification are required.'],
['Field and external benchmark evidence', 'NOT_EVALUATED', 'Repository arithmetic anchors are available; no measured installation comparison is supplied.']],
'Table 4. Assumptions, evidence gaps and required engineering actions.')}
<h3>10.2 Interface coverage</h3>
{table(['Block', 'Inputs / outputs', 'Treatment'], [
['Method and case', 'Lay method; optional descriptive case metadata', 'The router selects J-lay or reel-lay and rejects unsupported methods.'],
['Pipe', 'Diameter, wall, steel/coating/water densities, coating, elastic modulus, yield strength and optional Poisson’s ratio', 'Explicit geometry and density are required; optional defaults are reported.'],
['J-lay configuration', 'Depth, angle, force and strain limits', 'Positive submerged weight and force closure are required.'],
['Reel configuration', 'Core radius, strain limits, removal fraction and assessment span', 'Residual response is prescribed by the geometric assumption.'],
['Operability', 'Laying, joining and recovery cutoffs, calm rate and degradation', 'Optional; recovery cutoff is reported without a recovery assessment.'],
['Output destination', 'Directory and summary filename', 'A relative directory is resolved against configuration location or working directory.'],
['Returned mapping', 'Geometry, strain, stress proxy, selected limits, utilisation, defaults and screening status', 'Engineering values are returned alongside a summary-file reference.'],
['Summary file', 'Strict JSON of the calculated results', 'Non-finite serialization is rejected before directory creation; the destination is excluded from this report’s embedded record.']],
'Table 5. Workflow interface coverage and limits.')}
<p>The detailed J-lay result quantities are retained below.</p>
{result_table(record, 'jlay', jfields)}
<p>The detailed reel-lay result quantities are retained below.</p>
{result_table(record, 'reel', rfields)}
<h3>10.3 Quality pre-flight</h3>
<p>The displayed quantities are bound to source inputs or module returns. The rebuilt artifact is checked for
complete worked-example numerical bindings, local reference resolution, heading structure, self-contained resources
and unchanged deterministic output. Interactive points expose case, component, demand, cutoff and units without
colour-only interpretation. Browser appearance and file-permission prompts remain untested.</p>
<p>Supporting regression execution counts and environment limitations are retained with the accompanying change;
the static evidence register is not overwritten with execution claims. No licensed solver is imported by the report generator.</p>
<h3>10.4 Engineering changes</h3>
<details><summary>Changes at this revision</summary><ul id="change-list">
{''.join('<li>' + ESC(item) + '</li>' for item in changes()['changes'])}</ul></details>
<h3>10.5 Comments</h3>
<p>Select a report section and enter a comment. Save beside the HTML as <code>report.comments.json</code>.
Folder access verifies the selected report before writing. Where folder access is unavailable, the save dialog
or download is used; select the report’s folder. Comments are retained only after saving and loading the separate file.</p>
<label for="comment-section">Section</label> <select id="comment-section"></select>
<label for="comment-text">Comment</label><textarea id="comment-text" rows="4" style="display:block;width:100%"></textarea>
<button id="add-comment" type="button">Add comment</button>
<button id="save-comments" type="button">Save comments beside report</button>
<label for="load-comments">Load saved comments</label><input id="load-comments" type="file" accept="application/json">
<p id="comment-status" role="status" aria-live="polite">No comments have been saved.</p><ul id="comment-list"></ul>
</section>
'''


REVIEW_SCRIPT = r'''
(() => {
  'use strict';
  const config = JSON.parse(document.getElementById('review-config').textContent);
  const sections = [...document.querySelectorAll('main > section')];
  const sectionIds = new Set(sections.map(section => section.id));
  const select = document.getElementById('comment-section');
  const status = document.getElementById('comment-status');
  let comments = [];
  const message = text => { status.textContent = text; };
  const digest = async text => [...new Uint8Array(await crypto.subtle.digest(
    'SHA-256', new TextEncoder().encode(text.replace(/\r\n?/g, '\n'))))].map(n => n.toString(16).padStart(2, '0')).join('');
  const stripConfig = text => text.replace(/<script id="review-config" type="application\/json">[\s\S]*?<\/script>/, '');
  for (const section of sections) {
    const option = document.createElement('option');
    option.value = section.id;
    option.textContent = section.querySelector('h2').textContent;
    select.appendChild(option);
  }
  function display() {
    const list = document.getElementById('comment-list');
    list.replaceChildren();
    for (const comment of comments) {
      const item = document.createElement('li');
      item.textContent = `${comment.section}: ${comment.text}`;
      list.appendChild(item);
    }
  }
  function validate(incoming) {
    if (!incoming || incoming.revision !== config.revision ||
        incoming.report_digest !== config.report_digest || !Array.isArray(incoming.comments) ||
        incoming.comments.some(c => !c || !sectionIds.has(c.section) ||
          typeof c.text !== 'string' || !c.text.trim()))
      throw new Error('Comments do not match this report and revision.');
  }
  function merge(incoming) {
    validate(incoming);
    comments = [...new Map([...comments, ...incoming.comments].map(c => [JSON.stringify(c), c])).values()];
    display();
  }
  document.getElementById('add-comment').onclick = () => {
    const input = document.getElementById('comment-text');
    if (!input.value.trim()) return message('Enter a comment before adding.');
    comments.push({section: select.value, text: input.value.trim()});
    input.value = '';
    display();
    message('Comment added locally. Save the separate file to retain it.');
  };
  document.getElementById('load-comments').onchange = async event => {
    try {
      const file = event.target.files[0];
      if (!file) return;
      if (file.size > 5000000) throw new Error('Comments file exceeds the supported size.');
      const incoming = JSON.parse(await file.text());
      merge(incoming);
      message('Matching comments loaded. Save to retain any combined comments.');
    } catch (error) { message(error.message); }
    event.target.value = '';
  };
  document.getElementById('save-comments').onclick = async () => {
    try {
      let handle;
      if (typeof window.showDirectoryPicker === 'function') {
        const folder = await window.showDirectoryPicker({mode: 'readwrite'});
        const report = await folder.getFileHandle('report.html');
        const reportText = await (await report.getFile()).text();
        if (await digest(stripConfig(reportText)) !== config.report_digest)
          throw new Error('Selected folder contains a different report.');
        handle = await folder.getFileHandle('report.comments.json', {create: true});
      } else if (typeof window.showSaveFilePicker === 'function') {
        handle = await window.showSaveFilePicker({suggestedName: 'report.comments.json',
          types: [{description: 'Report comments', accept: {'application/json': ['.json']}}]});
      } else {
        const data = JSON.stringify({...config, comments}, null, 2) + '\n';
        const url = URL.createObjectURL(new Blob([data], {type: 'application/json'}));
        const link = document.createElement('a');
        link.href = url; link.download = 'report.comments.json'; link.click();
        setTimeout(() => URL.revokeObjectURL(url), 1000);
        return message('Comments download requested. Retain the file beside the report.');
      }
      const previous = await handle.getFile();
      if (previous.size > 5000000) throw new Error('Comments file exceeds the supported size.');
      const previousText = await previous.text();
      if (previousText.trim()) merge(JSON.parse(previousText));
      const data = JSON.stringify({...config, comments}, null, 2) + '\n';
      const writable = await handle.createWritable();
      await writable.write(data); await writable.close();
      if (await (await handle.getFile()).text() !== data) throw new Error('Saved comments differ on readback.');
      message('Comments saved and read back.');
    } catch (error) { message(error.message); }
  };
  const pointStatus = document.getElementById('point-status');
  for (const point of document.querySelectorAll('[data-point]')) {
    const show = () => {
      pointStatus.textContent = point.getAttribute('aria-label');
    };
    point.addEventListener('focus', show);
    point.addEventListener('pointerenter', show);
  }
})();
'''


def render(record):
    """Render one portable HTML file and bind comments to its full content."""
    payload = json.dumps(record, ensure_ascii=False, allow_nan=False, sort_keys=True)
    body = content(record)
    page = ('<!DOCTYPE html>\n<html lang="en" data-theme="light"><head><meta charset="utf-8">'
            '<meta name="viewport" content="width=device-width, initial-scale=1">'
            '<title>Pipelay module analysis</title>' + house_style() + '</head><body class="hs-doc">'
            '<div class="wrap"><header><div class="kicker">Marine installation</div>'
            '<h1>Pipelay module analysis</h1><p class="sub">Draft for maintainer review</p></header>'
            '<main>' + body + '</main><p id="point-status" role="status" aria-live="polite">'
            'Hover or focus a chart point to read its assumed rate and cutoff.</p></div>'
            '<script id="calculation-record" type="application/json">' + payload + '</script>'
            '<script id="engineering-changes" type="application/json">' + json.dumps(changes(), ensure_ascii=False)
            + '</script><script>' + REVIEW_SCRIPT + '</script></body></html>\n')
    config = json.dumps({"revision": REVISION, "report_digest": hashlib.sha256(page.encode("utf-8")).hexdigest()})
    # Digest excludes only this configuration block, avoiding a self-referential hash.
    return page.replace('</body>', '<script id="review-config" type="application/json">' + config + '</script></body>')


def build(output_dir, scratch):
    """Write deterministic deliverables; intermediate outputs stay in scratch."""
    if Path(scratch).resolve().is_relative_to(ROOT):
        raise ValueError('Use an external scratch directory for calculation summaries')
    record = calculate(scratch)
    page = render(record)
    output_dir = Path(output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)
    (output_dir / 'report.html').write_text(page, encoding='utf-8', newline='\n')
    (output_dir / 'changes.json').write_text(json.dumps(changes(), ensure_ascii=False, indent=2) + '\n',
                                          encoding='utf-8', newline='\n')
    return record


def main():
    """Require explicit scratch storage for all router summaries."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--output-dir', type=Path, required=True)
    parser.add_argument('--scratch-dir', type=Path, required=True)
    args = parser.parse_args()
    build(args.output_dir, args.scratch_dir)
    print('Built report.html and changes.json from analytical module calls.')


if __name__ == '__main__':
    main()
