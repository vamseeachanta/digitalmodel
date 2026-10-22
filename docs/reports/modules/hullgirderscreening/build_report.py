# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Build the standalone hull-girder module report from the public example.

From the repository root, with NumPy and PyYAML available:
    PYTHONPATH=src python docs/reports/modules/hullgirderscreening/build_report.py \
        --output-dir docs/reports/modules/hullgirderscreening --calculation-dir "$TMPDIR/hull-calculation"

Calculation CSVs are retained separately from the report deliverables.
"""

from __future__ import annotations

import argparse
import ast
import copy
import hashlib
import json
import math
import re
import sys
from html import escape
from pathlib import Path

import yaml

ROOT = Path(__file__).resolve().parents[4]
if str(ROOT / "src") not in sys.path:
    sys.path.insert(0, str(ROOT / "src"))

from digitalmodel.hull_girder_screening import router
from digitalmodel.naval_architecture.hull_girder_screening import DEFAULT_CLOSURE_TOLERANCE

EXAMPLE = "examples/workflows/hull-girder-report-pack/screening_input.yml"
BASIS = "examples/workflows/hull-girder-report-pack/allowable-basis.json"
EXEMPLAR = "docs/api/cfd/turbulent-flat-plate-verification.html"
TESTS = (
    "tests/naval_architecture/test_hull_girder_screening.py",
    "tests/naval_architecture/test_hull_girder_screening_workflow.py",
    "tests/naval_architecture/test_hull_girder_strength.py",
    "tests/report_pack/test_hull_girder_report_pack_demo.py",
)
CHANGES = {
    "report": "Hull-girder screening module analysis",
    "revision": "0",
    "history": [{
        "revision": "0", "date": "2026-10-22",
        "changes": [
            "Initial issue: document the still-water workflow and its analytical evidence.",
            "Distinguish illustrative allowable comparisons from strength acceptance.",
        ],
    }],
}


def format_number(value: float) -> str:
    """Use engineering precision without rounding a nonzero result to zero."""
    if not math.isfinite(value):
        raise ValueError("nonfinite report number")
    text = f"{value:.1e}" if 0 < abs(value) < 0.05 else f"{value:,.1f}"
    return text.replace("-", "−")


def calculate(calculation_dir: Path) -> dict:
    """Call the documented router with the still-water subset of the example."""
    inputs = yaml.safe_load((ROOT / EXAMPLE).read_text())["hull_girder_screening"]
    # The main workflow is still-water demand and supplied-curve screening.
    # Optional rule-wave and section-modulus sub-areas are documented as exclusions.
    inputs.pop("wave_loads")
    inputs.pop("section_modulus")
    inputs["allowables"] = {k: v for k, v in inputs["allowables"].items()
                           if not k.startswith("total_")}
    settings = copy.deepcopy(inputs)
    settings["output_dir"] = str(calculation_dir.resolve())
    result = router({"hull_girder_screening": settings})["hull_girder_screening"]
    published = {k: result[k] for k in (
        "length_m", "n_stations", "displacement_t", "lcg_m", "max_shear_t",
        "max_sagging_t_m", "x_max_sagging_m", "max_shear_utilization",
        "max_bending_utilization", "closure_shear_fraction", "closure_moment_fraction",
        "closure_ok", "screening_status", "utilization_limit", "stations", "utilization",
    )}
    length = inputs["length_m"]
    block = inputs["weights"]["lightship"][0]
    if (len(inputs["weights"]["lightship"]) != 1
            or set(inputs["weights"]) != {"lightship"}
            or block["x_start_m"] != length / 4
            or block["x_end_m"] != 3 * length / 4
            or block.get("lcg_m", length / 2) != length / 2
            or inputs["buoyancy"] != {"method": "box"}):
        raise ValueError("analytical comparator requires the documented central-block fixture")
    basis = json.loads((ROOT / BASIS).read_text())
    comparison = {}
    for key, actual, reference, tolerance in (
        ("shear", abs(result["max_shear_t"]), block["weight_t"] / 4,
         basis["validation"]["shear_relative_tolerance"]),
        ("moment", abs(result["max_sagging_t_m"]), block["weight_t"] * length / 16,
         basis["validation"]["bending_relative_tolerance"]),
    ):
        difference = 100 * (actual - reference) / reference
        comparison[key] = {"calculated": actual, "reference": reference,
                           "difference_percent": difference, "tolerance_percent": tolerance * 100}
        if abs(difference) > tolerance * 100:
            raise ValueError("analytical comparison exceeds the existing fixture tolerance")
    if not result["closure_ok"]:
        raise ValueError("example fails the raw closure gate")
    inventory = []
    for path in TESTS:
        tree = ast.parse((ROOT / path).read_text())
        inventory.append({"path": path, "definitions": sum(
            isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef))
            and node.name.startswith("test_") for node in ast.walk(tree))})
    return {"inputs": inputs, "result": published, "comparison": comparison,
            "closure_tolerance": DEFAULT_CLOSURE_TOLERANCE, "inventory": inventory,
            "strength_disposition": "NOT_EVALUATED"}


def table(headers: list[str], rows: list[list[str]], caption: str, key: str) -> str:
    """Render a table whose cells have already been escaped or generated."""
    return (f'<table id="{key}"><caption>{escape(caption)}</caption><thead><tr>'
            + "".join(f'<th scope="col">{escape(h)}</th>' for h in headers)
            + "</tr></thead><tbody>"
            + "".join("<tr>" + "".join(f"<td>{c}</td>" for c in row) + "</tr>" for row in rows)
            + "</tbody></table>")


def house_style() -> str:
    """Inline the existing canonical tokens and report layout without restyling."""
    brand = (ROOT / "docs/api/_assets/brand.css").read_text()
    exemplar = (ROOT / EXEMPLAR).read_text()
    layout = re.search(r"<style>(.*?)</style>", exemplar, re.S).group(1)
    return "<style>\n" + brand + "\n" + layout + "\n" + """
svg{max-width:100%;height:auto} .chart-controls{display:flex;gap:12px;flex-wrap:wrap}
textarea{display:block;width:100%;min-height:5em} button,select,input{font:inherit}
.comment-card{border-left:3px solid var(--accent);padding:8px 16px;margin:12px 0}
.chart-readout{min-height:5em} caption{text-align:left;color:var(--mut)}
@media print{button,textarea,input,.chart-controls{display:none}.wrap{box-shadow:none}}
</style>"""


def schematic(data: dict, number) -> str:
    """Draw the documented prismatic hull and its actual longitudinal loading."""
    block = data["inputs"]["weights"]["lightship"][0]
    length = data["inputs"]["length_m"]
    start = 80 + 620 * block["x_start_m"] / length
    end = 80 + 620 * block["x_end_m"] / length
    arrows = "".join(f'<path d="M{x} 270v-42m-5 8 5-8 5 8"/>' for x in range(100, 700, 40))
    return f"""<figure id="figure-geometry"><svg viewBox="0 0 780 340" role="img" aria-labelledby="geometry-title">
<title id="geometry-title">Elevation of the synthetic box barge and free-floating beam idealisation</title>
<defs><pattern id="cargo-hatch" width="8" height="8" patternUnits="userSpaceOnUse"><path d="M0 8 8 0" stroke="currentColor"/></pattern></defs>
<g fill="none" stroke="currentColor"><path d="M80 125V225H700V125H80" stroke-width="2"/>
<rect x="{start}" y="128" width="{end-start}" height="75" fill="url(#cargo-hatch)"/>
<path d="M45 190H735" stroke-dasharray="8 4"/>{arrows}
<path d="M80 78H700M80 68V88M700 68V88M80 288H700"/>
</g><g fill="currentColor" font-size="15"><text x="255" y="65">Length {number('inputs.length_m')} m</text>
<text x="260" y="112">Central cargo {number('inputs.weights.lightship.0.weight_t')} t</text>
<text x="85" y="310">Aft end, x increases forward →</text>
<text x="230" y="260">Upward distributed buoyancy</text>
<text x="80" y="38">Free ends: no applied end force or end couple</text></g></svg>
<figcaption>Figure 4.1. Cargo extends from {number('inputs.weights.lightship.0.x_start_m')} m to
{number('inputs.weights.lightship.0.x_end_m')} m. The dashed line indicates the waterline; depth and draft
are not represented to scale. No seabed or rigid support is included. Geometry is taken from
<a href="#source-example">Reference 9.2(a)</a>.</figcaption></figure>"""


def render(data: dict) -> str:
    """Write the complete report with the calculation and module evidence in context."""
    def number(path, scale=1, tag="span"):
        value = data
        for part in path.split("."):
            value = value[int(part)] if isinstance(value, list) else value[part]
        return (f'<{tag} data-number="{path}" data-scale="{scale}">'
                f'{format_number(value * scale)}</{tag}>')

    r = data["result"]
    input_rows = [
        ["Hull length", number("inputs.length_m"), "m", "9.2(a)"],
        ["Central cargo weight", number("inputs.weights.lightship.0.weight_t"), "t", "9.2(a)"],
        ["Cargo start", number("inputs.weights.lightship.0.x_start_m"), "m", "9.2(a)"],
        ["Cargo end", number("inputs.weights.lightship.0.x_end_m"), "m", "9.2(a)"],
        ["Integration stations", number("inputs.n_stations"), "count", "9.2(a)"],
        ["Raw end-closure tolerance", number("closure_tolerance", 100), "%", "9.2(b)"],
        ["Illustrative utilisation limit", number("inputs.utilization_limit", 100), "%", "9.2(a)"],
    ]
    frame_rows = []
    for i, _ in enumerate(r["utilization"]):
        p = f"result.utilization.{i}."
        frame_rows.append([number(p + "x_m"), number(p + "shear_force_t"),
                           number(p + "shear_allowable_t"), number(p + "bending_moment_t_m"),
                           number(p + "bending_allowable_t_m"), number(p + "shear_utilization", 100),
                           number(p + "bending_utilization", 100), "Table 10.2"])
    curve_rows = []
    for name, curve in data["inputs"]["allowables"].items():
        field = "positive_t" if name == "shear_force" else "hogging_t_m"
        for i in range(len(curve["x_m"])):
            p = f"inputs.allowables.{name}."
            curve_rows.append(["Shear force" if name == "shear_force" else "Bending moment",
                               number(p + f"x_m.{i}"), number(p + f"{field}.{i}"),
                               "t" if name == "shear_force" else "t·m", "9.2(a)"])
    compare_rows = []
    for key, label, unit in (("shear", "Shear magnitude", "t"), ("moment", "Sagging magnitude", "t·m")):
        p = f"comparison.{key}."
        compare_rows.append([label, number(p + "calculated"), number(p + "reference"), unit,
                             number(p + "difference_percent"), number(p + "tolerance_percent"), "9.2(c), 9.2(d)"])
    inventory_rows = [[label, number(f"inventory.{i}.definitions"), "9.2(c)" if i == 0 else "9.2(e)"]
                      for i, label in enumerate(("Core screening", "Routed workflow", "Supporting strength", "Report-pack example"))]
    revision = CHANGES["history"][0]
    # The prose is authored as one report; source locators are confined to 9.2.
    body = f"""
<header><div class="kicker">Module analysis · Longitudinal strength</div>
<h1>Hull-girder screening module analysis</h1><p class="sub">Draft for client review</p></header>
<nav aria-label="Contents"><p>Contents:
<a href="#section-1">Executive summary</a> · <a href="#section-2">Introduction</a> ·
<a href="#section-3">Summary</a> · <a href="#section-4">Design data</a> ·
<a href="#section-5">Methodology</a> · <a href="#section-6">Detailed results</a> ·
<a href="#section-7">Validation</a> · <a href="#section-8">Findings and recommendations</a> ·
<a href="#section-9">References</a> · <a href="#section-10">Appendices</a></p></nav>
<section id="section-1"><h2>1. Executive Summary</h2>
<p>The hull-girder screening module is a longitudinal loading-condition calculator. Analysis is performed
to determine still-water shear force, bending moment and comparisons with supplied allowable curves;
key results are summarized below.</p>
<ul><li>Loading conditions are ranked using calculated demand and raw equilibrium closure.
<ul><li>The synthetic central-cargo example gives a maximum sagging moment of
{number('result.max_sagging_t_m')} t·m at {number('result.x_max_sagging_m')} m.</li>
<li>The numerical comparison is limited by the analytical-fixture tolerances in Table 7.1;
it does not establish strength acceptance.</li></ul></li>
<li>Illustrative checks satisfy the supplied example limits.
<ul><li>The maximum bending utilisation is {number('result.max_bending_utilization', 100)} % against the
{number('result.utilization_limit', 100)} % illustrative limit; strength acceptance is NOT_EVALUATED.</li>
<li>Approved loading-manual limits and an independent loading-instrument comparison are required before
an operational strength decision is made.</li></ul></li></ul>
<p>The demand and illustrative boundary are shown in Figure 6.1. The scope and disposition are supported
by <a href="#source-example">References 9.2(a)</a>, <a href="#source-core">9.2(b)</a> and
<a href="#source-basis">9.2(d)</a>.</p>
<div id="revision-history"><h3>Revision history</h3>
{table(['Revision', 'Date', 'What changed and why'], [[escape(revision['revision']), escape(revision['date']),
'Initial issue: explain the loading calculation and separate numerical agreement from strength acceptance.']], 'Report issue record, newest first.', 'revision-table')}</div></section>
<section id="section-2"><h2>2. Introduction</h2>
<p>The main still-water workflow is documented for engineers reviewing loading conditions. A documented
public synthetic box-barge example is recalculated through the module entry point; it is not a measured
asset or a transcription of a textbook numerical example.</p>
<p>The implementation builds distributed and concentrated weights, obtains buoyancy, integrates the net
load and compares demands with position-dependent allowable magnitudes. The result is exported as
station, frame and summary tables. Source functions and their test records are listed in
<a href="#source-core">Section 9.2</a>.</p></section>
<section id="section-3"><h2>3. Summary</h2>
<p>The loading calculation is used as a screening aid. Controls are applied to the input basis and the
reported coverage before any loading condition is referred for an operational decision.</p>
<p>Weight units, positions, total weight and longitudinal centre of gravity are checked against the loading
record. The buoyancy method is selected for the actual hull; the box method is restricted here to the
prismatic example. Raw end residuals are reviewed before corrected load curves are interpreted.</p>
<p>Both sign-specific allowable curves are supplied from the approved loading manual. Missing negative
curves inherit positive magnitudes; this symmetry is an example assumption, not evidence of an approved
limit. All reporting frames and expected demand peaks are included. A returned pass status alone does
not establish that every strength check was evaluated: omitted curves produce no-allowable frame
results, while the overall status can still be pass. These behaviours are recorded in
<a href="#source-router">Reference 9.2(f)</a>.</p></section>
<section id="section-4"><h2>4. Design Data</h2>
<p>The calculation basis is the central-cargo box-barge example in <a href="#source-example">Reference 9.2(a)</a>.
Still-water geometry, weights, frames and allowable curves are copied without changing their values.</p>
<h3>4.1 Physical model and inputs</h3>
{schematic(data, lambda path: number(path, tag="tspan"))}
{table(['Input', 'Value', 'Unit', 'Source'], input_rows, 'Table 4.1. Calculation inputs and numerical gate.', 'table-inputs')}
<p>Distributed lightship and tank blocks require weight, longitudinal extent and an optional centre of
gravity; an omitted centre is placed at the extent midpoint. Concentrated weights require weight and
position, with an optional spreading extent. The current calculation contains only the central cargo
block. A hydrostatics-table alternative requires immersed sectional area against draft and position;
a direct alternative requires buoyancy per length against position. Table 10.2 records the example curves.</p>
<h3>4.2 Criteria and qualification</h3>
<p>Raw end shear divided by displacement and raw end moment divided by displacement times length are
compared with the closure tolerance in Table 4.1. This is a configurable numerical-balance gate from
<a href="#source-core">Reference 9.2(b)</a>, not a class strength criterion. Corrected closure alone does
not qualify the original buoyancy. Frame utilisation is compared with the illustrative utilisation limit
in Table 4.1; these synthetic allowable curves do not establish a permissible operational loading envelope.</p>
<p>ASSUMED - to be confirmed: the synthetic hull, cargo and symmetric example limits are suitable only
for demonstrating the calculation. No loading-manual approval is represented by the example values.
The qualification gaps and required evidence are listed in Table 10.1.</p></section>
<section id="section-5"><h2>5. Methodology</h2>
<p>The longitudinal hull is treated as a free-floating beam. Net load is integrated along the hull using
cumulative trapezoidal quadrature, with the sign convention defined below.</p>
<h3>5.1 Equilibrium, integration and signs</h3>
<div class="eq">q(x) = w(x) − b(x); V(x) = ∫₀ˣ q(s) ds; M(x) = ∫₀ˣ V(s) ds</div>
<p>The coordinate x is measured forward from the aft end in metres; s is the integration coordinate.
Weight density w and buoyancy density b are in tonnes per metre. The net density q is weight minus
buoyancy; shear V is in mass-equivalent tonnes and moment M is in mass-equivalent tonne-metres.
Positive M is hogging; negative M is sagging. The general beam equilibrium relations are given in
<a href="#source-theory">Reference 9.2(g)</a>, equations (3.68) and (3.72). That reference uses the opposite
load sign: its q is b − w when mapped to this convention. The implemented convention is stated in
<a href="#source-core">Reference 9.2(b)</a>.</p>
<p>For the box method, a linear buoyancy distribution is fitted to the discrete weight and longitudinal
moment. Optional linear closure correction is applied after the raw residuals are recorded. Distributed
weights are represented by cell-averaged linear densities constrained by total weight and centre of
gravity; a centre outside the middle third of a block is rejected. Concentrated weights are spread
rather than treated as exact force jumps. These implementation choices are documented in
<a href="#source-core">Reference 9.2(b)</a>.</p>
<h3>5.2 Allowable interpolation and analytical comparator</h3>
<div class="eq">u<sub>V</sub>(x) = |V(x)| / V<sub>allow</sub>(x);
u<sub>M</sub>(x) = |M(x)| / M<sub>allow</sub>(x)</div>
<p>Utilisations u are dimensionless. Allowable magnitudes are linearly interpolated at the reporting
frames, and the allowable matching the demand sign is used. Outside the supplied allowable knot range,
the end allowable is held constant. The module does not search a continuous utilisation envelope
between reporting frames; the chart markers identify the positions actually screened.</p>
<div class="eq">|V|<sub>reference</sub> = W/4; M<sub>reference</sub>(L/2) = −WL/16</div>
<p>For the documented central block over the middle half of a hull of length L and cargo weight W,
these continuous solutions follow by integrating the piecewise constant net density. Their derivation
and numerical comparison criteria are held in <a href="#source-tests">Reference 9.2(c)</a> and
<a href="#source-basis">9.2(d)</a>. A spread block edge changes the sampled shear extreme; the continuous
reference is therefore a comparator rather than an expected identical discrete value.</p>
<h3>5.3 Entry point and outputs</h3>
<p>The public dictionary entry point is imported and called as follows. Configuration is deep-copied
before the call because the router replaces the settings block with results.</p>
<pre><code>from copy import deepcopy
from digitalmodel.hull_girder_screening import router
result = router(deepcopy(config))["hull_girder_screening"]</code></pre>
<p>The configuration contains a hull-girder settings mapping and an explicit output directory.
Station tables contain position, weight density, buoyancy density, shear and moment. Frame tables
contain demand, allowable, utilisation and status. Summary tables contain extremes and positions,
raw residuals and maxima of the evaluated utilisations. Comma-separated-value files and the returned
dictionary carry these results; numerical output paths are kept out of this report.</p></section>
<section id="section-6"><h2>6. Detailed Results</h2>
<p>The public example is computed through the router with its still-water inputs. Optional wave loads,
total allowable curves and section-modulus inputs are excluded from this worked example.</p>
<h3>6.1 Worked example</h3>
<p>The calculated displacement is {number('result.displacement_t')} t and its longitudinal centre of gravity
is {number('result.lcg_m')} m. The largest shear has signed value {number('result.max_shear_t')} t.
The maximum sagging moment is {number('result.max_sagging_t_m')} t·m at
{number('result.x_max_sagging_m')} m. Raw end shear and moment residuals are respectively
{number('result.closure_shear_fraction', 100)} % and {number('result.closure_moment_fraction', 100)} %,
against the {number('closure_tolerance', 100)} % closure gate. The raw closure gate is satisfied.</p>
<p>The maximum frame shear utilisation is {number('result.max_shear_utilization', 100)} % and the maximum
frame bending utilisation is {number('result.max_bending_utilization', 100)} %. Both use the
{number('result.utilization_limit', 100)} % illustrative threshold. These comparisons demonstrate
arithmetic with supplied curves; operational strength acceptance remains NOT_EVALUATED.</p>
<div class="chart-controls"><label>Quantity <select id="chart-quantity"><option value="bending">Bending moment</option>
<option value="shear">Shear force</option></select></label><label>Position along hull
<input id="chart-position" type="range" min="0" max="{len(r['stations'])-1}" value="{len(r['stations'])//2}"></label></div>
<figure id="figure-demand"><svg id="demand-chart" viewBox="0 0 780 360" role="img" aria-labelledby="chart-title">
<title id="chart-title">Demand against illustrative positive and negative allowable magnitudes</title>
<g fill="none" stroke="currentColor"><path d="M70 25V295H750"/>
<path id="chart-limit" stroke-dasharray="7 4"/><path id="chart-negative-limit" stroke-dasharray="7 4"/>
<path id="chart-demand" stroke="var(--brand)" stroke-width="2"/>
<path id="chart-cursor" stroke="var(--accent)"/></g>
<g id="chart-markers" fill="var(--brand)"></g><g id="chart-labels" fill="currentColor" font-size="13"></g>
<text x="270" y="345" fill="currentColor">Position along hull (m)</text></svg>
<figcaption>Figure 6.1. One demand quantity is shown against longitudinal position. Solid line: calculated
demand; dashed lines: illustrative boundaries; dots: screened frames. Hover or use the position control
to read demand and the sign-specific limit. Curves are from Table 10.2; evaluated frames are in Table 10.3.</figcaption></figure>
<p id="chart-readout" class="chart-readout" aria-live="polite"></p>
<p>The legend is interpreted as follows. The numerical gate is satisfied when raw closure is inside its
tolerance. An illustrative check is satisfied when evaluated utilisation is inside its supplied limit.
Strength acceptance is not evaluated when qualified limits or independent reference evidence are absent.</p>
<noscript><p>The interactive chart requires JavaScript. Tables 7.1 and 10.3 retain the numerical results.</p></noscript>
</section>
<section id="section-7"><h2>7. Validation</h2>
<p>Analytical comparison and existing regression evidence are distinguished from operational validation.
The results below compare the current discrete calculation with the continuous central-block solution.</p>
{table(['Quantity', 'Calculated', 'Reference', 'Unit', 'Difference (%)', 'Absolute difference limit (%)', 'Source'], compare_rows,
'Table 7.1. Signed differences relative to the continuous analytical reference.', 'table-comparison')}
<p>Both comparisons satisfy the unrounded existing fixture criteria. Nonzero differences are preserved
when they are too small for ordinary fixed-point display. The criteria test numerical integration,
not structural adequacy.</p>
{table(['Evidence area', 'Test definitions', 'Source'], inventory_rows, 'Table 7.2. Existing test-function inventory, not executed-case counts.', 'table-inventory')}
<p>The core tests include sagging and mirrored hogging signs, symmetry, weight integrals, invalid extents,
a box-hull waterline recovery, deliberately unbalanced direct buoyancy, allowable exceedance and section
properties. Optional wave tests exercise closed-form formula reproduction and combinations. The workflow
tests exercise parsing, tables and statuses; the report-pack tests compare regenerated example data
with committed data. Parametrisation can produce more cases than the definition counts in Table 7.2.</p>
<p>No approved loading-instrument comparison or field-data validation is held in the cited example
basis. Reproducing the same formula or committed table does not provide an independent check of the
rule edition or an operational asset. Missing independent evidence is recorded in
<a href="#source-basis">Reference 9.2(d)</a>.</p></section>
<section id="section-8"><h2>8. Key Findings, Limitations and Recommendations</h2>
<p>The implemented still-water workflow is supported by a synthetic analytical comparison and automated
regression records. Engineering acceptance remains dependent on a qualified loading basis.</p>
<h3>8.1 Findings and recommendations</h3>
<p>The central-block result has the documented sagging sign and satisfies the numerical comparison
criteria in Table 7.1. Approved loading-manual curves, their sign conventions and frame coverage are
required for an asset-specific screen. A comparison with an approved loading instrument is required
before the result is used to set an operational limit, consistent with
<a href="#source-guide">Reference 9.2(h)</a>.</p>
<p>When a raw closure gate fails, the weight and buoyancy force and moment balance is reconciled before
any corrected result is used. When an illustrative allowable is exceeded, the loading condition is
referred for engineering assessment and load redistribution is evaluated using approved limits;
this report does not assign an operating allowance. These actions follow the closure and screening
basis in <a href="#source-core">References 9.2(b)</a> and <a href="#source-guide">9.2(h)</a>.</p>
<p>Station refinement and additional reporting frames are used to resolve load discontinuities and
utilisation peaks. Finite numeric values, units, monotonic direct-buoyancy positions, curve coverage
and boolean settings are checked before calling the router: the implementation does not comprehensively
validate all such inputs. A missing allowable is treated as missing evidence rather than acceptance.</p>
<h3>8.2 Limits of use and not covered</h3>
<p>The box buoyancy idealisation does not replace hydrostatics for a shaped hull. Load discontinuities
are spread by cell averaging; local stresses and exact concentrated-load jumps are not represented.
The closure correction is a numerical device, not evidence that supplied hydrostatics are physically
valid. Results are sampled at stations and frame gates; no continuous maximum-utilisation search is made.</p>
<p>The optional hydrostatics-table waterline solve and direct-buoyancy import are described but not
worked here. Rule wave envelopes, total still-water-plus-wave screening, scantling-derived section
properties and section-modulus yield screening are not covered by the worked example. The supporting
strength module also contains an ultimate-strength estimate, which is outside this router workflow.
Fatigue, buckling, torsion, horizontal bending, slamming, whipping and installation sea-state envelopes
are not evaluated. Orientation limits, crane rigging and forecast support are not applicable to this
still-water module analysis; no sea-state operating limit is inferred.</p>
<p>The implementation names IACS UR S11, Longitudinal Strength Standard, Rev.7 (2010), wave bending
moment and wave shear force clauses. Edition equivalence and the permissible-stress clause basis are
not established here. No licensed numerical rule coefficient or limit is reproduced.</p></section>
<section id="section-9"><h2>9. References</h2>
<p>The calculation basis is separated from the general beam-theory reference. Repository records are
used for the example values and for statements about implemented behaviour.</p>
<h3>9.1 Source qualification</h3>
<p>The repository example is synthetic and publicly documented. The beam notes are an open educational
source, Fall 2013 edition; only the equilibrium relations are used. The unavailable textbook editions
named by existing tests are not used as numerical authorities. The source register below identifies
the exact repository records and symbols used by this report.</p>
<h3>9.2 Source register and reproduction</h3>
<ul><li id="source-example">(a) <a href="../../../../{EXAMPLE}">Public synthetic example, still-water fields</a>:
length, cargo, station count, frames and placeholder curves. Wave, total and section-modulus fields
are excluded from this calculation.</li>
<li id="source-core">(b) <a href="../../../../src/digitalmodel/naval_architecture/hull_girder_screening.py">Core calculation</a>:
DistributedWeight, build_weight_curve, buoyancy_box, still_water_sf_bm, utilization_at_frames and
DEFAULT_CLOSURE_TOLERANCE. This is the source for the numerical closure setting and the sign convention.</li>
<li id="source-tests">(c) <a href="../../../../{TESTS[0]}">Analytical regression fixture</a>:
test_sagging_barge_matches_hand_calc and its module-level derivation. The continuous shear and moment
expressions in Section 5.2 are stated there.</li>
<li id="source-basis">(d) <a href="../../../../{BASIS}">Allowable evidence and validation basis</a>:
validation.shear_relative_tolerance and validation.bending_relative_tolerance; qualification of missing
approved curves and independent evidence. Synthetic limits remain unqualified.</li>
<li id="source-dependent-tests">(e) <a href="../../../../{TESTS[1]}">Workflow tests</a>;
<a href="../../../../{TESTS[2]}">supporting strength tests</a>;
<a href="../../../../{TESTS[3]}">report-pack example tests</a>.
Table 7.2 is counted from test definitions in these files and Reference (c).</li>
<li id="source-router">(f) <a href="../../../../src/digitalmodel/hull_girder_screening/workflow.py">Routed entry point</a>:
router, _screening_status and _frames. Overall pass can coexist with absent strength checks.</li>
<li id="source-theory">(g) <a href="https://ocw.mit.edu/courses/2-080j-structural-mechanics-fall-2013/resources/mit2_080jf13_lecture3/">MIT OpenCourseWare, Structural Mechanics, Fall 2013, Lecture 3</a>,
Section 3.6, equations (3.68), (3.72) and (3.73). Load sign is mapped explicitly in Section 5.1.
The institutional page and open notes were read on 2026-10-22.</li>
<li id="source-guide">(h) <a href="../../../domains/ship-design/hull-girder-strength-screening.md">Module usage guide</a>:
screening scope, analytical example and missing loading-instrument reference fixture.</li>
<li id="source-style">(i) <a href="../../../api/cfd/turbulent-flat-plate-verification.html">Existing report layout</a>
and <a href="../../../api/_assets/brand.css">canonical stylesheet</a>. Their styles are inlined in this report;
the requested hs-doc body class is applied.</li></ul>
<p>Reproduction entry: <code>docs/reports/modules/hullgirderscreening/build_report.py</code>, with explicit
<code>--output-dir</code> and <code>--calculation-dir</code> arguments. The latter retains calculation tables
outside the report directory. Rebuild assertions are in
<code>tests/naval_architecture/test_hull_girder_module_report.py</code>. Numerical display is generated
from returned values; raw values are retained in the embedded calculation data.</p></section>
<section id="section-10"><h2>10. Appendices</h2>
<p>The input qualification register and detailed frame values are retained for engineering review.
Comments are saved separately so the issued calculation remains unchanged.</p>
<h3>10.1 Data register</h3>
{table(['Data', 'Basis', 'Required evidence', 'Source'], [
['Hull and central cargo', 'Synthetic public example; ASSUMED - to be confirmed for asset use', 'Actual loading condition and hull hydrostatics', '9.2(a)'],
['Allowable curves and sign symmetry', 'Illustrative placeholders; ASSUMED - to be confirmed', 'Approved loading manual, edition, condition and both signs', '9.2(a), 9.2(d)'],
['Closure and numerical comparison limits', 'Numerical settings, not strength criteria', 'Resolution assessment for each new loading pattern', 'Table 4.1; Table 7.1'],
['Operational strength acceptance', 'NOT_EVALUATED', 'Qualified inputs and independent loading-instrument comparison', '9.2(d), 9.2(h)'],
], 'Table 10.1. Data qualification and evidence gaps.', 'table-register')}
<h3>10.2 Detailed numerical basis</h3>
{table(['Quantity', 'Position (m)', 'Allowable magnitude', 'Unit', 'Source'], curve_rows,
'Table 10.2. Illustrative allowable knots; negative magnitudes inherit positive values.', 'table-curves')}
{table(['Position (m)', 'Shear (t)', 'Shear limit (t)', 'Moment (t·m)', 'Moment limit (t·m)', 'Shear (%)', 'Bending (%)', 'Source'], frame_rows,
'Table 10.3. Router-returned still-water frame results. Limits are illustrative.', 'table-frames')}
<h3>10.3 Quality evidence</h3>
<p>The rebuild test recalculates the public example independently, compares every published station and
frame field with the router, checks numerical text and verifies reproducible HTML and change records.
The test-definition inventory is derived from the cited files. These assertions establish reproducibility
of this report; they do not establish independent validation of an operational hull.</p>
<p>The quality pre-flight basis is stated in Tables 4.1, 7.1 and 10.1: input provenance, numerical criteria,
sign mapping, missing data and intended use. Every example number is generated from the configuration,
router output or analytical comparator. No finite-element calculation, mesh or licensed solver is used.</p>
<h3>10.4 Engineering changes and comments</h3>
<details id="changes-panel"><summary>Engineering changes since the previous issue</summary>
<p>Initial issue. The still-water calculation, analytical comparison and missing operational evidence
are documented together. No earlier engineering result is superseded.</p></details>
<p>Comments are attached to a section and optional selected quotation. Save the comment file beside this
HTML as report.comments.json. Where direct file saving is unavailable, the downloaded file is placed
beside the report manually. Import that file to continue; a browser download alone does not confirm its location.</p>
<label>Section <select id="comment-section"></select></label>
<label>Quotation <textarea id="comment-quote"></textarea></label>
<button id="comment-selection" type="button">Use selected report text</button>
<label>Comment <textarea id="comment-text"></textarea></label>
<button id="comment-add" type="button">Add comment</button>
<button id="comment-save" type="button">Save comments</button>
<label>Import comments <input id="comment-import" type="file" accept=".json,application/json"></label>
<p id="comment-message" aria-live="polite">Comments are retained in this page until saved.</p>
<div id="comment-list"></div>
</section>
<footer>Hull-girder screening module analysis · Revision {escape(CHANGES['revision'])} · Synthetic example · Draft for client review</footer>
"""
    payload = json.dumps(data, ensure_ascii=False, allow_nan=False, separators=(",", ":")).replace("</", "<\\/")
    changes = json.dumps(CHANGES, ensure_ascii=False).replace("</", "<\\/")
    page = ('<!DOCTYPE html>\n<html lang="en" data-theme="light"><head><meta charset="utf-8">\n'
            '<meta name="viewport" content="width=device-width, initial-scale=1">\n'
            '<title>Hull-girder screening module analysis</title>\n' + house_style()
            + '\n</head><body class="hs-doc"><main class="wrap">' + body + '</main>\n'
            + '<script type="application/json" id="calculation-data">' + payload + '</script>\n'
            + '<script type="application/json" id="engineering-changes">' + changes + '</script>\n'
            + '<script>\n' + INTERACTION + '\n</script></body></html>\n')
    return page.replace("REPORT_BINDING", hashlib.sha256(page.encode()).hexdigest())


INTERACTION = r"""
(() => {
  'use strict';
  const el = id => document.getElementById(id);
  const data = JSON.parse(el('calculation-data').textContent);
  const stations = data.result.stations, frames = data.result.utilization;
  const num = value => {
    const text = value !== 0 && Math.abs(value) < 0.05 ? value.toExponential(1) :
      value.toLocaleString('en-US', {minimumFractionDigits: 1, maximumFractionDigits: 1});
    return text.replaceAll('-', '−');
  };
  const lerp = (xs, ys, x) => {
    if (x <= xs[0]) return ys[0];
    for (let i = 1; i < xs.length; i++) {
      if (x <= xs[i]) return ys[i-1] + (ys[i]-ys[i-1]) * (x-xs[i-1]) / (xs[i]-xs[i-1]);
    }
    return ys[ys.length-1];
  };
  const svgNode = (name, attrs, text) => {
    const node = document.createElementNS('http://www.w3.org/2000/svg', name);
    Object.entries(attrs).forEach(([key, value]) => node.setAttribute(key, value));
    if (text !== undefined) node.textContent = text;
    return node;
  };
  let chartState;
  function chart() {
    const bending = el('chart-quantity').value === 'bending';
    const key = bending ? 'bending_moment_t_m' : 'shear_force_t';
    const quantity = bending ? 'Bending moment' : 'Shear force';
    const units = bending ? 't·m' : 't';
    const curve = data.inputs.allowables[bending ? 'bending_moment' : 'shear_force'];
    const positive = curve[bending ? 'hogging_t_m' : 'positive_t'];
    const negative = curve[bending ? 'sagging_t_m' : 'negative_t'] || positive;
    const limit = (x, sign) => lerp(curve.x_m, sign < 0 ? negative : positive, x);
    const scale = Math.max(...positive, ...negative, ...stations.map(s => Math.abs(s[key]))) * 1.08;
    const x = value => 70 + 680 * value / data.inputs.length_m;
    const y = value => 160 - 130 * value / scale;
    const path = values => values.map(([px, py], i) => `${i ? 'L' : 'M'}${x(px)},${y(py)}`).join('');
    el('chart-demand').setAttribute('d', path(stations.map(s => [s.x_m, s[key]])));
    el('chart-limit').setAttribute('d', path(stations.map(s => [s.x_m, limit(s.x_m, 1)])));
    el('chart-negative-limit').setAttribute('d', path(stations.map(s => [s.x_m, -limit(s.x_m, -1)])));
    el('chart-markers').replaceChildren(...frames.map(f => svgNode('circle', {cx:x(f.x_m), cy:y(f[key]), r:4})));
    el('chart-labels').replaceChildren(
      svgNode('text', {x:72,y:18}, `${quantity} (${units})`),
      ...[-1, 0, 1].map(k => svgNode('text', {x:5,y:y(k*scale)+4}, num(k*scale))),
      ...[0, 0.25, 0.5, 0.75, 1].map(k => svgNode('text', {x:x(k*data.inputs.length_m)-10,y:320}, num(k*data.inputs.length_m))));
    chartState = {key, quantity, units, limit, x};
    readout();
  }
  function readout() {
    const s = stations[Number(el('chart-position').value)], c = chartState;
    const value = s[c.key], limit = c.limit(s.x_m, value);
    el('chart-cursor').setAttribute('d', `M${c.x(s.x_m)} 25V295`);
    el('chart-readout').textContent = `Case: central cargo. Component: longitudinal hull girder. ${c.quantity}. ` +
      `Position: ${num(s.x_m)} m. Demand: ${num(value)} ${c.units}. Illustrative limit magnitude: ${num(limit)} ${c.units}. ` +
      `Strength acceptance is not evaluated. Values between dots are display samples rather than additional frame gates.`;
  }
  el('chart-quantity').addEventListener('change', chart);
  el('chart-position').addEventListener('input', readout);
  el('demand-chart').addEventListener('pointermove', event => {
    const svg = el('demand-chart'), point = svg.createSVGPoint();
    point.x = event.clientX; point.y = event.clientY;
    const local = point.matrixTransform(svg.getScreenCTM().inverse());
    const fraction = Math.max(0, Math.min(1, (local.x - 70) / 680));
    el('chart-position').value = Math.round(fraction * (stations.length - 1));
    readout();
  });
  chart();
  const revision = JSON.parse(el('engineering-changes').textContent).revision;
  const binding = 'REPORT_BINDING';
  const sections = [...document.querySelectorAll('section[id]')];
  const sectionIds = new Set(sections.map(s => s.id));
  sections.forEach(s => {
    const option = document.createElement('option'); option.value = s.id;
    option.textContent = s.querySelector('h2').textContent; el('comment-section').append(option);
  });
  let comments = [];
  const message = text => { el('comment-message').textContent = text; };
  function showComments() {
    el('comment-list').replaceChildren(...comments.map(item => {
      const div = document.createElement('div'); div.className = 'comment-card';
      for (const text of [item.section, item.quote, item.text]) {
        const p = document.createElement('p'); p.textContent = text; div.append(p);
      }
      return div;
    }));
  }
  el('comment-selection').onclick = () => {
    const selection = window.getSelection();
    if (!selection || !selection.rangeCount) return message('Select report text first.');
    const node = selection.anchorNode;
    const section = (node.nodeType === 1 ? node : node.parentElement).closest('section');
    if (!section || !sectionIds.has(section.id)) return message('Select text inside a numbered section.');
    el('comment-section').value = section.id;
    el('comment-quote').value = selection.toString();
  };
  el('comment-add').onclick = () => {
    const text = el('comment-text').value.trim();
    if (!text) return message('Enter a comment before adding it.');
    comments.push({section:el('comment-section').value, quote:el('comment-quote').value, text});
    el('comment-text').value = ''; showComments(); message('Comment added. Save comments to retain it.');
  };
  el('comment-save').onclick = async () => {
    const text = JSON.stringify({report:'hull-girder-screening', revision, binding, comments}, null, 2) + '\n';
    try {
      if (typeof window.showSaveFilePicker === 'function') {
        const handle = await window.showSaveFilePicker({suggestedName:'report.comments.json',
          types:[{description:'Comment file',accept:{'application/json':['.json']}}]});
        const writable = await handle.createWritable(); await writable.write(text); await writable.close();
        if (await (await handle.getFile()).text() !== text) throw new Error('Saved comments differ on read-back.');
        message('Comment file saved and read back.');
      } else {
        const url = URL.createObjectURL(new Blob([text], {type:'application/json'}));
        const link = document.createElement('a'); link.href = url; link.download = 'report.comments.json';
        link.click(); setTimeout(() => URL.revokeObjectURL(url), 1000);
        message('Download requested. Place the file beside the report and import it to check retention.');
      }
    } catch (error) { message(error.message); }
  };
  el('comment-import').onchange = async event => {
    try {
      const file = event.target.files[0]; if (!file) return;
      const saved = JSON.parse(await file.text());
      if (saved.report !== 'hull-girder-screening' || saved.revision !== revision || saved.binding !== binding ||
          !Array.isArray(saved.comments) || !saved.comments.every(c => c && sectionIds.has(c.section) &&
          typeof c.quote === 'string' && typeof c.text === 'string')) throw new Error('Comment file does not match this report.');
      comments = saved.comments; showComments(); message('Comments imported from the selected file.');
    } catch (error) { message(error.message); }
  };
})();
"""


def build(output_dir: Path, calculation_dir: Path) -> dict:
    """Write only the report and change panel beside one another."""
    if output_dir.resolve() == calculation_dir.resolve():
        raise ValueError("calculation tables require a separate directory")
    data = calculate(calculation_dir)
    output_dir.mkdir(parents=True, exist_ok=True)
    (output_dir / "report.html").write_text(render(data), encoding="utf-8")
    (output_dir / "changes.json").write_text(
        json.dumps(CHANGES, indent=2, ensure_ascii=False) + "\n", encoding="utf-8")
    return data


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", type=Path, required=True)
    parser.add_argument("--calculation-dir", type=Path, required=True)
    args = parser.parse_args()
    build(args.output_dir, args.calculation_dir)
