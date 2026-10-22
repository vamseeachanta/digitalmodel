# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Build the offline fluid-column report from public inputs and module calls.

Run from the repository root: PYTHONPATH=src python
docs/reports/modules/hydrostatics/build_report.py
Use --output-dir to rebuild outside the documentation tree for comparison.
Only report.html and changes.json are written; review comments are preserved.
"""

from __future__ import annotations

import argparse
import ast
import hashlib
import html
import inspect
import json
import re
from pathlib import Path

from digitalmodel import hydrostatics as hs

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[3]
CHANGES = {
    "report": "Fluid-column hydrostatics analysis",
    "revisions": [{
        "revision": "1", "date": "2026-10-22",
        "changes": [
            "Static water-column pressure is calculated from a published example.",
            "Gauge and absolute pressure are distinguished; structural acceptance is not evaluated.",
            "The existing calculation checks and limits of application are documented."
        ],
        "reason": "Establish a cited calculation basis and limits for engineering use."
    }]
}


def inventory():
    """Read the current public functions and focused test definitions."""
    def count_tests(path):
        tree = ast.parse(path.read_text())
        return sum(isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef))
                   and node.name.startswith("test_") for node in ast.walk(tree))

    return {
        "public_functions": sum(inspect.isfunction(getattr(hs, name)) for name in hs.__all__),
        "existing_tests": count_tests(ROOT / "tests/hydrostatics/test_fluid_column.py"),
        "report_tests": count_tests(ROOT / "tests/hydrostatics/test_module_report.py"),
    }


def calculate():
    """Call the module; retain full precision until values are displayed."""
    basis = json.loads((HERE / "basis.json").read_text())
    values = dict(basis["inputs"])
    rho = values["density_kg_m3"]
    gravity = values["gravity_m_s2"]
    depth = values["evaluation_depth_m"]
    values["pressure_pa"] = hs.hydrostatic_pressure(rho, depth, gravity)
    values["bottom_pa"] = hs.seabed_pressure(
        rho, values["water_depth_m"], values["surface_gauge_pa"], gravity
    )
    values["seabed_at_evaluation_pa"] = hs.seabed_pressure(
        rho, depth, values["surface_gauge_pa"], gravity
    )
    values["pressure_psi"] = hs.pa_to_psi(values["pressure_pa"])
    values["pressure_bar"] = hs.psi_to_bar(values["pressure_psi"])
    values["default_pressure_pa"] = hs.hydrostatic_pressure(rho, depth)
    values["reference_pa"] = basis["reference"]["average_gauge_pressure_pa"]
    values["reference_difference_pct"] = (
        (values["pressure_pa"] - values["reference_pa"]) / values["reference_pa"] * 100
    )
    values["default_difference_pct"] = (
        (values["default_pressure_pa"] - values["reference_pa"]) / values["reference_pa"] * 100
    )
    profile = []
    for index in range(17):
        h = values["water_depth_m"] * index / 16
        pressure = hs.hydrostatic_pressure(rho, h, gravity)
        profile.append({
            "depth_m": h, "pressure_pa": pressure,
            "description": f"Public water column; static water; depth {h:.1f} m; "
                           f"demand {pressure / 1000:.1f} kPa; "
                           "limit NOT_EVALUATED (no allowable supplied)."
        })
    return {
        "values": values, "profile": profile, "inventory": inventory(),
        "allowable_pressure_pa": None, "absolute_pressure_pa": None,
    }


def table(headers, rows, caption):
    """Render real columns with an engineering caption."""
    head = "".join(f"<th scope=\"col\">{item}</th>" for item in headers)
    body = "".join("<tr>" + "".join(f"<td>{item}</td>" for item in row) + "</tr>" for row in rows)
    return f"<table><caption>{caption}</caption><thead><tr>{head}</tr></thead><tbody>{body}</tbody></table>"


def render():
    """Write the report narrative with the complete module in context."""
    data = calculate()
    values = data["values"]
    counts = data["inventory"]

    def number(key, scale=1, fmt=".1f"):
        raw = values[key]
        display = format(raw / scale, fmt).replace("-", "−")
        return (f'<span data-value="{key}" data-raw="{raw}" data-scale="{scale}" '
                f'data-format="{fmt}">{display}</span>')

    def cite():
        return '<a href="#public-source">Public example basis</a>'

    brand = (ROOT / "docs/api/_assets/brand.css").read_text()
    exemplar = (ROOT / "docs/api/cfd/turbulent-flat-plate-verification.html").read_text()
    style = re.search(r"<style>.*?</style>", exemplar, re.S).group(0)
    inputs = table(
        ["Input", "Value", "Unit", "Source"],
        [
            ["Water density, ρ", number("density_kg_m3"), "kg/m³", cite()],
            ["Gravity, g", number("gravity_m_s2"), "m/s²", cite()],
            ["Water depth, H", number("water_depth_m"), "m", cite()],
            ["Evaluation depth, h", number("evaluation_depth_m"), "m", cite()],
            ["Surface gauge pressure", number("surface_gauge_pa"), "Pa", "Pressure-difference datum; " + cite()],
        ], "Table A. Public input data register; values apply to the educational example only."
    )
    results = table(
        ["Quantity", "Value", "Unit", "Source"],
        [
            ["Pressure increment at evaluation depth", number("pressure_pa", 1000), "kPa", "Table A; pressure function"],
            ["Bottom helper at evaluation depth", number("seabed_at_evaluation_pa", 1000), "kPa", "Table A; bottom-pressure function"],
            ["Pressure increment at reservoir bottom", number("bottom_pa", 1000), "kPa", "Table A; bottom-pressure function"],
            ["Converted evaluation pressure", number("pressure_psi"), "psi", "Table A; module conversion"],
            ["Converted evaluation pressure", number("pressure_bar"), "bar", "Table A; module conversion"],
            ["Published reference pressure", number("reference_pa", 1000), "kPa", cite()],
            ["Difference from published reference", number("reference_difference_pct"), "%", "Table A; computed relative to reference"],
            ["Evaluation pressure with module default gravity", number("default_pressure_pa", 1000), "kPa", "Table A; default gravity in module"],
            ["Default-gravity difference from reference", number("default_difference_pct"), "%", "Table A; computed relative to reference"],
        ], "Table B. Calculated pressure and reference comparison. Comparisons use unrounded values."
    )
    conversions = table(
        ["Conversion", "Multiplier (output unit per input unit)", "Source"],
        [["ft to m", f"{hs.FT_TO_M:.3e}", '<a href="#implementation-source">Module constants</a>'],
         ["ppg to kg/m³", f"{hs.PPG_TO_KG_M3:.3e}", '<a href="#implementation-source">Module constants</a>'],
         ["psi to Pa", f"{hs.PSI_TO_PA:.3e}", '<a href="#implementation-source">Module constants</a>']],
        "Table C. Implementation multipliers at display precision; full precision is used in calculations."
    )
    records = table(
        ["Item", "Status and engineering action", "Source"],
        [["Density and gravity", "Published example inputs; replace with the applicable fluid and location before design use.", "Table A"],
         ["Absolute surface pressure", "NOT_EVALUATED. Supply an absolute surface pressure before reporting an absolute bottom pressure.", "Table A; pressure datum"],
         ["Structural allowable pressure", "NOT_EVALUATED. Supply the component rating and its governing design basis before acceptance is assessed.", "Table B; pressure demand only"],
         ["Variable density", "NOT_EVALUATED. Obtain density as a function of depth when the constant-density approximation is not applicable.", '<a href="#public-source">Public theory source</a>'],
         ["Assumed engineering inputs", "None. No missing physical input is replaced by an assumed value.", "Table A"]],
        "Table D. Missing-data register and actions."
    )
    profile_rows = [[f'{p["depth_m"]:.1f}', f'{p["pressure_pa"] / 1000:.1f}', "Table A; pressure function"]
                    for p in data["profile"]]
    profile_table = table(["Depth (m)", "Gauge pressure (kPa)", "Source"], profile_rows,
                          "Table E. Diagram samples; the analytical solution is continuous between samples.")
    changes = html.escape(" ".join(CHANGES["revisions"][0]["changes"]))
    body = f"""
<header><div class="kicker">Engineering module analysis</div>
<h1>Fluid-column hydrostatics analysis</h1><p class="sub">Draft for client review</p></header>
<nav aria-label="Report contents"><p><a href="#executive">Executive summary</a> ·
<a href="#basis">Design data</a> · <a href="#example">Worked example</a> ·
<a href="#validation">Validation</a> · <a href="#appendix">Appendices</a> ·
<a href="#comments">Comments</a></p></nav>

<section id="executive"><h2>1 Executive Summary</h2>
<p>Fluid-column hydrostatics is a static pressure calculation for a liquid of uniform density.
Analysis is performed to determine the pressure increase below a stated surface datum;
key results are summarized below.</p>
<ul><li>The published water-column pressure is reproduced with the stated gravity.
<ul><li>At {number("evaluation_depth_m")} m, the computed gauge pressure is
{number("pressure_pa", 1000)} kPa; the difference from the published reference is
{number("reference_difference_pct")} %. {cite()}.</li></ul></li>
<li>The result is a pressure demand for subsequent component assessment.
<ul><li>Allowable pressure and absolute pressure are NOT_EVALUATED because a component
rating and an absolute surface pressure are not supplied. The pressure diagram carries
the surface boundary; it establishes no operating limit.</li></ul></li></ul>
<h3>Revision history</h3>
<table><thead><tr><th>Revision</th><th>Date</th><th>What changed and why</th></tr></thead>
<tbody><tr><td>1</td><td>2026-10-22</td><td>A cited public pressure example and application
limits are introduced to establish the calculation basis.</td></tr></tbody></table>
</section>

<section id="introduction"><h2>2 Introduction</h2>
<p>The pressure increase in a stationary liquid is calculated from density, gravity and
vertical depth. The calculation is applied to the pressure part of a published reservoir
example; component strength and installation conditions are outside this assessment.</p>
<p>The public interface contains {counts['public_functions']} functions: pressure increase,
bottom pressure and conversions for density, length and pressure. The pressure calculation
has no dependency on a licensed analysis program. The existing implementation and tests
are identified in <a href="#sources">the evidence register</a>.</p>
</section>

<section id="controls"><h2>3 Summary and calculation controls</h2>
<p>A consistent pressure datum is required for the surface and bottom of the column.
Engineering acceptance is left unevaluated until the component rating is supplied.</p>
<p>Density is entered in kilograms per cubic metre, height in metres and gravity in metres
per second squared. Pressure is returned in pascals. A supplied gauge surface pressure
produces a gauge bottom pressure; an absolute surface pressure produces an absolute result.
The default surface datum is gauge despite the bottom helper's absolute-pressure wording.</p>
<p>The caller is responsible for finite values, physical density and a non-negative depth
when depth is measured down from the surface. Input validation, datum tagging and unit
checking are not implemented. Negative inputs and non-finite arithmetic are not rejected.</p>
<p>The conversion helpers accept density in pounds per US gallon (ppg), length in feet (ft)
and pressure in pounds per square inch (psi), bar or pascals (Pa), as appropriate to the
selected helper. Each returns the converted numerical value without a unit wrapper.</p>
</section>

<section id="basis"><h2>4 Design data and criteria</h2>
<p>The calculation basis is taken from the published example without substituting missing
engineering inputs. Equality to its pressure result is the reproduction criterion;
structural acceptance is not inferred from that equality.</p>
<p>The liquid density is {number("density_kg_m3")} kg/m³ and gravity is
{number("gravity_m_s2", fmt='.2f')} m/s². Reservoir depth H is
{number("water_depth_m")} m and evaluation depth h is
{number("evaluation_depth_m")} m. {cite()}.</p>
<figure><svg viewBox="0 0 760 330" role="img" aria-labelledby="geometry-title geometry-desc">
<title id="geometry-title">Reservoir cutaway and static pressure boundary</title>
<desc id="geometry-desc">Water rests against a dam face. Depth is measured down from the
free surface. Pressure arrows grow with depth. The model uses constant density and gravity.</desc>
<defs><marker id="arrow" markerWidth="8" markerHeight="8" refX="7" refY="4" orient="auto-start-reverse">
<path d="M0,0 L8,4 L0,8 Z" fill="currentColor"/></marker></defs>
<path d="M70 270 L70 95 L465 95 L465 270 Z" fill="var(--panel-2)" stroke="var(--brand)"/>
<path d="M70 95 Q100 85 130 95 T190 95 T250 95 T310 95 T370 95 T430 95 L465 95"
fill="none" stroke="var(--accent)" stroke-width="3"/>
<path d="M465 75 L505 75 L575 270 L465 270 Z" fill="var(--edge)" stroke="var(--muted)"/>
<path d="M55 270 L620 270" stroke="var(--muted)" stroke-width="5"/>
<g stroke="var(--brand)" stroke-width="2" marker-end="url(#arrow)">
<path d="M435 125 L462 125"/><path d="M410 175 L462 175"/><path d="M380 225 L462 225"/>
<path d="M640 95 L640 270" marker-start="url(#arrow)"/>
<path d="M180 95 L180 182" marker-start="url(#arrow)"/></g>
<g fill="var(--ink)" font-size="17"><text x="90" y="55">Surface pressure p₀; free surface</text>
<text x="205" y="160">Water at rest; constant ρ and g</text>
<text x="190" y="190">h</text><text x="650" y="190">H</text>
<text x="490" y="310">Dam face</text><text x="80" y="310">Reservoir bed</text></g>
</svg><figcaption>Figure A. Physical model and boundary conditions. The cutaway is schematic;
only the water depths are specified by the source. No dam geometry or strength is assessed.</figcaption></figure>
<p>The acceptance comparison uses the source gravity explicitly. The module default is
{hs.G_STANDARD:.2f} m/s², as recorded by the <a href="#implementation-source">implementation</a>;
it is not a location-specific gravity measurement. No density or depth range is enforced.</p>
</section>

<section id="theory"><h2>5 Theory and methodology</h2>
<p>Hydrostatic equilibrium is integrated for a constant-density liquid under uniform gravity.
The resulting pressure increment is added to the supplied surface pressure.</p>
<div class="eq">dp/dz = −ρg; Δp = ρgh; p<sub>b</sub> = p₀ + Δp</div>
<p>The upward coordinate is z, downward depth is h, density is ρ, gravitational acceleration
is g, surface pressure is p₀ and bottom pressure is p<sub>b</sub>. Δp is the pressure increment.
These relations and the constant-density restriction are given by the
<a href="#public-source">public theory source</a>.</p>
<p>The pressure function evaluates Δp directly. The bottom-pressure function adds the
surface value. No atmospheric pressure is added automatically. Conversions are scalar
multiplications; they do not identify the datum or change the physical model.</p>
<p>Numerical arrays with multiplication and addition semantics are supported by the
arithmetic and are exercised by an existing test. Ordinary Python lists are not a general
array input contract. Stratified liquids require pressure increments to be integrated
using their actual density distribution; that integration is not supplied by this module.</p>
</section>

<section id="example"><h2>6 Worked public example</h2>
<p>The pressure part of the published reservoir example is reproduced by calling the public
pressure function. The mean wall pressure equals the pressure at mid-depth because the
pressure distribution is linear in depth.</p>
<p>With the input basis in Table A, the function returns {number("pressure_pa", 1000)} kPa
at {number("evaluation_depth_m")} m. The published reference is
{number("reference_pa", 1000)} kPa, giving a difference of
{number("reference_difference_pct")} %. The bottom pressure at
{number("water_depth_m")} m is {number("bottom_pa", 1000)} kPa gauge. {cite()}.</p>
<p>The module default gravity gives {number("default_pressure_pa", 1000)} kPa at the same
evaluation depth, a difference of {number("default_difference_pct")} % from the published
reference. The explicit source gravity is retained for reproduction.</p>
<figure><svg id="pressure-chart" viewBox="0 0 760 380" role="img" aria-labelledby="chart-title chart-desc">
<title id="chart-title">Gauge pressure against depth</title>
<desc id="chart-desc">The pressure is linear with depth. The dashed surface boundary is
zero gauge pressure. No allowable pressure is supplied. All samples are in Table E.</desc>
<path d="M80 35 L80 310 L710 310" stroke="var(--ink)" fill="none"/>
<path d="M80 310 L710 310" stroke="var(--muted)" stroke-dasharray="8 5"/>
<polyline id="pressure-line" fill="none" stroke="var(--brand)" stroke-width="3"/>
<g id="pressure-points"></g><circle id="selected-point" r="7" fill="var(--accent)"/>
<g fill="var(--ink)" font-size="16"><text x="285" y="365">Depth below surface (m)</text>
<text transform="translate(23 250) rotate(-90)">Gauge pressure (kPa)</text>
<text x="84" y="335">Surface</text><text x="590" y="335">Reservoir bed</text></g></svg>
<p>The plotted depth range extends from the surface to {number("water_depth_m")} m;
gauge pressure ranges from {number("surface_gauge_pa")} to {number("bottom_pa", 1000)} kPa.</p>
<label for="depth-sample">Select a depth sample</label>
<input id="depth-sample" type="range" min="0" max="16" step="1" value="8" aria-describedby="chart-readout">
<p id="chart-readout" aria-live="polite"></p>
<figcaption>Figure B. Static water pressure demand. Hover or focus a sample for depth, demand,
component and units. The dashed line is the surface boundary, not a capacity limit.
Allowable pressure is NOT_EVALUATED. <a href="#profile-data">Table E</a> provides the data.</figcaption></figure>
<p>Pressure units other than pascals are obtained through the module converters. The
converted values and default-gravity comparison are recorded in Table B. The force part
of the published example is excluded because wall-force integration is not a module function.</p>
</section>

<section id="validation"><h2>7 Validation evidence</h2>
<p>Calculation-level evidence is provided by the existing focused tests and the public
pressure comparison. Field measurements and pressure-rating qualification are not held
in the inspected module test set.</p>
<p>The existing set contains {counts['existing_tests']} tests: unit conversions and round trips,
the fluid-column relation, array arithmetic, a worksheet pressure comparison and the
zero-surface gauge convention. The worksheet inputs are described as generic in that
test file; the underlying spreadsheet is not provided as independently inspectable evidence.
These checks establish the stated assertions only. They do not establish a design envelope.</p>
<p>The report is checked by {counts['report_tests']} additional tests. Module outputs are compared
with the cited public reference, every bound displayed example value and diagram sample
is checked, and the rebuilt report is compared byte for byte with the issued page.
The source records and commands are given in <a href="#sources">the evidence register</a>.</p>
<p>No mesh, time step or convergence iteration is used by this closed-form calculation.
Array agreement checks arithmetic consistency, rather than independent physical validation.</p>
</section>

<section id="recommendations"><h2>8 Findings, limitations and recommendations</h2>
<p>The published pressure calculation is reproduced when its inputs and gravity are used.
Application is limited by the constant-density, static-fluid model and by caller-managed units.</p>
<h3>Recommendations</h3>
<p>Record the pressure datum with every input and result; this follows the surface-plus-increment
relation in the <a href="#public-source">theory source</a>. Before a component acceptance decision,
supply its rating and applicable design basis, and compare that rating with the pressure demand.
Acceptance remains NOT_EVALUATED in their absence.</p>
<p>Obtain a density profile when temperature, salinity or compressibility makes density vary
with depth. Integrate that profile using a suitable model, as required by the
<a href="#public-source">variable-density equilibrium relation</a>. Accelerating fluids,
flow losses, transients and gas columns require additional governing physics.</p>
<p>Add finite-value, unit and datum checks at the calling interface before operational use.
Retain independent conversion references before changing the legacy pressure multiplier:
the existing tests preserve worksheet compatibility and round-trip consistency, rather than
qualification to a metrology reference. No production coefficient is changed by this analysis.</p>
</section>

<section id="appendix"><h2>9 Appendices</h2>
<p>The detailed input, output and evidence registers are provided here for reproduction.
Unprovided physical data are identified explicitly rather than assigned a value.</p>
<h3>9.1 Data and results registers</h3>{inputs}{results}{conversions}{records}
<div id="profile-data">{profile_table}</div>
<h3 id="sources">9.2 Sources and reproduction</h3>
<p id="public-source">Public source: OpenStax, University Physics Volume 1, published
19 September 2016; online edition accessed 22 October 2026. Section 14.1, Fluids, Density,
and Pressure; equations 14.4, 14.5 and the variable-density derivation; Table 14.2;
Example 14.1, What Force Must a Dam Withstand?, pressure part.
<a href="https://openstax.org/books/university-physics-volume-1/pages/14-1-fluids-density-and-pressure">Read the source</a>.
Inputs and pressure results are factual data; the discussion and schematic are newly written.</p>
<p id="implementation-source">Implementation source: the current repository edition of
<a href="../../../../src/digitalmodel/hydrostatics/fluid_column.py">fluid_column.py</a>
and its <a href="../../../../src/digitalmodel/hydrostatics/__init__.py">public exports</a>.
The gravity default and conversion constants in Table C are implementation data, not
newly qualified physical constants.</p>
<p>Existing evidence: <a href="../../../../tests/hydrostatics/test_fluid_column.py">fluid-column tests</a>.
Report checks: <a href="../../../../tests/hydrostatics/test_module_report.py">rebuild and value tests</a>.
Input record: <a href="basis.json">public example basis</a>.
Report source: <a href="build_report.py">generator</a>.
Style exemplar: <a href="../../../api/cfd/turbulent-flat-plate-verification.html">existing CFD report</a>
with <a href="../../../api/_assets/brand.css">canonical brand stylesheet</a>.
Both stylesheets are inlined in this page; the body uses the hs-doc class.</p>
<pre>PYTHONPATH=src python docs/reports/modules/hydrostatics/build_report.py
PYTHONPATH=src python -m pytest tests/hydrostatics -q -rA -p no:cacheprovider</pre>
<pre>from digitalmodel.hydrostatics import hydrostatic_pressure, seabed_pressure
pressure_pa = hydrostatic_pressure(density={number("density_kg_m3")},
                                   height={number("evaluation_depth_m")},
                                   gravity={number("gravity_m_s2", fmt='.2f')})
bottom_gauge_pa = seabed_pressure(density={number("density_kg_m3")},
                                  height={number("water_depth_m")},
                                  surface_pressure={number("surface_gauge_pa")},
                                  gravity={number("gravity_m_s2", fmt='.2f')})</pre>
<p>The pressure calculation is called with the density, height and gravity recorded in
Table A. The bottom helper is called with the same inputs and the stated surface gauge
datum. The generator supports an alternate output directory and leaves comments untouched.</p>
<h3>9.3 Quality pre-flight evidence</h3>
<p>The rebuild tests check example values against independent calls, diagram endpoints,
the missing-capacity status, document structure, references, inlined house style and
preservation of existing comment files. The test inventory is counted from the current
test definitions. The public page was retrieved successfully; its source metadata and
input basis are recorded in the linked input record. No operating acceptance is claimed.</p>
<details id="changes"><summary>Engineering changes since the previous issue</summary>
<p>{changes}</p><p>The initial issue establishes the calculation basis. There is no earlier
issued result to compare. The same record is supplied in <a href="changes.json">changes.json</a>.</p></details>
</section>

<section id="not-covered"><h2>10 Not covered</h2>
<p>Hull displacement, buoyancy centres, trim and intact stability are handled outside this
fluid-column module. They are excluded from the present assessment.</p>
<p>Dam-force integration, structural resistance, wave loading, station keeping and installation
sea-state envelopes are not calculated. Orientation tolerances, rigging and forecast thresholds
have no supplied basis in this static pressure example and are not assigned.</p>
</section>

<aside id="comments"><h3>Review comments</h3>
<p>Select a report section and record a comment. Save report.comments.json beside this HTML
file and load it to continue. The file picker supports direct saving; otherwise a download
is offered and the same folder is selected in the browser. Closing without saving loses edits.</p>
<label for="comment-section">Section</label><select id="comment-section"></select>
<label for="comment-text">Comment</label><textarea id="comment-text" rows="4"></textarea>
<p><button id="add-comment" type="button">Add comment</button>
<button id="save-comments" type="button">Save comments</button>
<label for="load-comments">Load comments</label><input id="load-comments" type="file" accept="application/json"></p>
<p id="comment-message" role="status"></p><ol id="comment-list"></ol></aside>
<footer>Static pressure demand only. Component acceptance and absolute pressure are NOT_EVALUATED.</footer>
"""
    payload = json.dumps(data, ensure_ascii=False, allow_nan=False)
    identity = hashlib.sha256("\n".join([body, brand, style, payload, INTERACTION]).encode()).hexdigest()
    return (f'<!DOCTYPE html>\n<html lang="en" data-theme="light"><head>\n'
            '<meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">\n'
            '<title>Fluid-column hydrostatics analysis</title>\n'
            f'<style>\n{brand}</style>\n{style}\n</head><body class="hs-doc"><div class="wrap">\n'
            + body + '\n</div>\n'
            + f'<script id="calculation-data" type="application/json">{payload}</script>\n'
            + f'<script id="review-identity" type="application/json">{json.dumps(identity)}</script>\n'
            + '<script>\n' + INTERACTION + '\n</script>\n</body></html>\n')


INTERACTION = r"""(() => {
  'use strict';
  const byId = id => document.getElementById(id);
  const data = JSON.parse(byId('calculation-data').textContent);
  const points = data.profile;
  const xy = point => [80 + 630 * point.depth_m / data.values.water_depth_m,
    310 - 275 * point.pressure_pa / data.values.bottom_pa];
  byId('pressure-line').setAttribute('points', points.map(p => xy(p).join(',')).join(' '));
  const select = index => {
    const point = points[index], [x, y] = xy(point);
    byId('selected-point').setAttribute('cx', x);
    byId('selected-point').setAttribute('cy', y);
    byId('chart-readout').textContent = point.description;
    byId('depth-sample').setAttribute('aria-valuetext', point.description);
  };
  points.forEach((point, index) => {
    const circle = document.createElementNS('http://www.w3.org/2000/svg', 'circle');
    const [x, y] = xy(point);
    for (const [key, value] of Object.entries({cx:x, cy:y, r:5, fill:'var(--brand)', tabindex:0,
      'aria-label':point.description})) circle.setAttribute(key, value);
    const title = document.createElementNS('http://www.w3.org/2000/svg', 'title');
    title.textContent = point.description;
    circle.appendChild(title);
    circle.addEventListener('mouseenter', () => select(index));
    circle.addEventListener('focus', () => select(index));
    byId('pressure-points').appendChild(circle);
  });
  byId('depth-sample').addEventListener('input', event => select(Number(event.target.value)));
  select(Number(byId('depth-sample').value));

  const report = 'fluid-column-hydrostatics', revision = '1';
  const identity = JSON.parse(byId('review-identity').textContent);
  const sections = [...document.querySelectorAll('section')];
  const ids = new Set(sections.map(section => section.id));
  let comments = [];
  sections.forEach(section => {
    const option = document.createElement('option');
    option.value = section.id;
    option.textContent = section.querySelector('h2').textContent;
    byId('comment-section').appendChild(option);
  });
  const message = text => { byId('comment-message').textContent = text; };
  const show = () => {
    byId('comment-list').replaceChildren();
    comments.forEach(comment => {
      const item = document.createElement('li');
      item.textContent = `${comment.section}: ${comment.text}`;
      byId('comment-list').appendChild(item);
    });
  };
  byId('add-comment').addEventListener('click', () => {
    const text = byId('comment-text').value.trim(), section = byId('comment-section').value;
    if (!text || !ids.has(section)) return message('Select a section and enter comment text.');
    comments.push({section, text});
    byId('comment-text').value = '';
    show(); message('Comment added. Save to retain it.');
  });
  byId('save-comments').addEventListener('click', async () => {
    const text = JSON.stringify({report, revision, identity, comments}, null, 2) + '\n';
    try {
      if (typeof window.showSaveFilePicker === 'function') {
        const handle = await window.showSaveFilePicker({suggestedName:'report.comments.json',
          types:[{description:'Report comments', accept:{'application/json':['.json']}}]});
        const writer = await handle.createWritable();
        await writer.write(text); await writer.close();
        if (await (await handle.getFile()).text() !== text) throw new Error('Comment file read-back differs.');
        message('Comments saved and read back.');
      } else {
        const url = URL.createObjectURL(new Blob([text], {type:'application/json'}));
        const anchor = document.createElement('a');
        anchor.href = url; anchor.download = 'report.comments.json'; anchor.click();
        setTimeout(() => URL.revokeObjectURL(url), 1000);
        message('Download offered. Save beside the report and load it to check the saved file.');
      }
    } catch (error) { message(error.name === 'AbortError' ? 'Save cancelled; comments remain open.' : error.message); }
  });
  byId('load-comments').addEventListener('change', async event => {
    try {
      const file = event.target.files[0];
      if (!file) return;
      const incoming = JSON.parse(await file.text());
      if (incoming.report !== report || incoming.revision !== revision || incoming.identity !== identity ||
        !Array.isArray(incoming.comments) || incoming.comments.some(comment =>
          !comment || !ids.has(comment.section) || typeof comment.text !== 'string'))
        throw new Error('Comments do not match this report content and revision.');
      const combined = [...comments, ...incoming.comments];
      comments = [...new Map(combined.map(comment => [JSON.stringify(comment), comment])).values()];
      show(); message('Matching comments loaded; existing unsaved comments are retained.');
    } catch (error) { message(error.message); }
    event.target.value = '';
  });
})();"""


def main():
    """Write only the report and engineering change record."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", type=Path, default=HERE)
    args = parser.parse_args()
    args.output_dir.mkdir(parents=True, exist_ok=True)
    (args.output_dir / "report.html").write_text(render(), encoding="utf-8")
    (args.output_dir / "changes.json").write_text(
        json.dumps(CHANGES, indent=2, ensure_ascii=False) + "\n", encoding="utf-8"
    )


if __name__ == "__main__":
    main()
