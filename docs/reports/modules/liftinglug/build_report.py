# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Build the offline lifting-lug analysis from the documented public example.

From the repository root, with the module's analytical dependencies installed::

    PYTHONPATH=src python docs/reports/modules/liftinglug/build_report.py \
        --output-dir REPORT_DIRECTORY --work-dir CALCULATION_DIRECTORY

Both directories are explicit so calculation summaries stay outside the source
tree. No simulation engine or licensed solver is used.
"""

from __future__ import annotations

import argparse
import copy
import json
import math
import re
from pathlib import Path

import yaml

from digitalmodel.lifting_lug import workflow

ROOT = Path(__file__).resolve().parents[4]
EXAMPLE = ROOT / "examples/workflows/lifting-lug-design/input.yml"
REVISION = "2026-10-21"
CHANGES = {
    "document": "Lifting lug screening analysis",
    "revisions": [{
        "revision": "A", "date": REVISION,
        "changes": [
            "The published example is evaluated for bearing, net tension and shear tear-out.",
            "A load restriction is calculated for the failed shear screen; design acceptance remains unevaluated.",
            "Unconfirmed material, loading and strength-factor bases are registered for engineering review.",
        ],
    }, {
        "revision": "B", "date": "2026-10-22",
        "changes": [
            "UTF-8 and LF output are explicit so the report rebuilds across platforms.",
            "Independent hand calculations check reported stresses, utilization, exceedance and load restriction.",
            "Conceptual tear-out planes, field precision and the load sensitivity heading are clarified for maintainer review.",
        ],
    }],
}
NAMES = {
    "pin_bearing": "Pin-hole bearing",
    "net_tension": "Net-section tension",
    "shear_tearout": "Shear tear-out",
}


def read_utf8_lf(path: Path) -> str:
    """Read source text with explicit encoding and LF newline handling."""
    with path.open(encoding="utf-8", newline="\n") as source:
        return source.read()


def house_style() -> str:
    """Inline existing brand tokens and the established report layout unchanged."""
    brand = read_utf8_lf(ROOT / "docs/api/_assets/brand.css")
    exemplar = read_utf8_lf(ROOT / "docs/api/cfd/turbulent-flat-plate-verification.html")
    layout = re.search(r"<style>(.*?)</style>", exemplar, re.S)[1]
    style = re.sub(r"/\*.*?\*/", "", brand + "\n" + layout, flags=re.S)
    return "\n".join(line.rstrip() for line in style.splitlines()).strip()


def evaluate(cfg: dict, directory: Path) -> dict:
    """Call the public router, with summary output confined to the work area."""
    cfg = copy.deepcopy(cfg)
    cfg["lifting_lug"]["outputs"]["directory"] = str(directory.resolve())
    return workflow.router(cfg)["lifting_lug"]


def numerical_fields(result: dict) -> dict:
    """Collect inputs and module outputs without retyping example values."""
    values = {f"lug.{key}": value for key, value in result["lug"].items()}
    values.update({f"load.{key}": value for key, value in result["load"].items()
                   if isinstance(value, (int, float))})
    for key in ("design_load_kN", "total_thickness_mm", "skew_load_factor",
                "skew_load_factor_minimum"):
        values[key] = result[key]
    for check in result["checks"]:
        for key in ("demand_mpa", "allowable_mpa", "utilization"):
            values[f"{check['name']}.{key}"] = check[key] * (100 if key == "utilization" else 1)
    values.update({
        "bearing_factor": workflow.BEARING_FACTOR,
        "tension_factor": workflow.TENSION_FACTOR,
        "shear_factor": workflow.SHEAR_FACTOR,
        "limit_percent": 100.0,
        "governing_percent": result["governing_utilization"] * 100,
        "exceedance_percent": (result["governing_utilization"] - 1) * 100,
        "ligament_mm": result["lug"]["outer_radius_mm"] - result["lug"]["hole_diameter_mm"] / 2,
        # Round down: a displayed operating restriction must not exceed the screen.
        "static_cap_kN": math.floor(result["load"]["static_load_kN"]
                                     / result["governing_utilization"] * 10) / 10,
    })
    return values


def chart_cases(cfg: dict, values: dict, directory: Path) -> dict:
    """Evaluate a one-variable load sweep with the router, including the example."""
    static = values["load.static_load_kN"]
    loads = {round(static * (0.25 + index / 24), 1) for index in range(25)}
    loads.update((static, values["static_cap_kN"]))
    cases = []
    for index, load in enumerate(sorted(loads)):
        case = copy.deepcopy(cfg)
        case["lifting_lug"]["load"]["static_load_kN"] = load
        result = evaluate(case, directory / f"load-{index:02d}")
        cases.append({
            "static_load_kN": load,
            "status": result["screening_status"],
            "checks": [{
                "component": check["name"],
                "demand_mpa": round(check["demand_mpa"], 1),
                "allowable_mpa": round(check["allowable_mpa"], 1),
                "utilization_percent": round(check["utilization"] * 100, 1),
            } for check in result["checks"]],
        })
    return {"limit_percent": values["limit_percent"], "cases": cases, "names": NAMES}


def schematic(values: dict, number) -> str:
    """Draw the example's round head and actual pin/hole proportions."""
    radius = values["lug.outer_radius_mm"]
    scale = 110 / radius
    hole = values["lug.hole_diameter_mm"] / 2 * scale
    pin = values["lug.pin_diameter_mm"] / 2 * scale
    return f'''<figure id="geometry">
<svg viewBox="0 0 820 390" role="img" aria-labelledby="geometry-title geometry-desc">
<title id="geometry-title">Circular lifting lug with axial in-plane pin loading and a side view</title>
<desc id="geometry-desc">The hole and pin are concentric. The loaded ligament extends from the hole to the round head.
The base reaction is illustrative; weld and parent plate resistance are outside the calculation.</desc>
<defs><marker id="arrow" viewBox="0 0 10 10" refX="8" refY="5" markerWidth="7" markerHeight="7" orient="auto-start-reverse">
<path d="M 0 0 L 10 5 L 0 10 z" fill="currentColor"/></marker>
<pattern id="hatch" width="10" height="10" patternUnits="userSpaceOnUse"><path d="M0 10 L10 0" stroke="currentColor"/></pattern></defs>
<path d="M120 305 L120 160 A110 110 0 0 1 340 160 L340 305 Z" fill="var(--soft)" stroke="var(--ink)" stroke-width="2"/>
<circle cx="230" cy="160" r="{hole:.2f}" fill="var(--panel)" stroke="var(--ink)" stroke-width="2"/>
<circle cx="230" cy="160" r="{pin:.2f}" fill="var(--line)" stroke="var(--ink)"/>
<path d="M230 160 L230 15" stroke="var(--brand)" stroke-width="3" marker-end="url(#arrow)"/>
<text x="244" y="29">Factored sling load P</text>
<path d="M230 160 L327 105" stroke="var(--ink)" marker-end="url(#arrow)"/>
<text x="333" y="104">Outer radius R</text>
<path d="M190 58 L190 160 M270 58 L270 160" stroke="var(--nogo)" stroke-dasharray="5 3"/>
<text x="45" y="66">Tear-out planes</text><path d="M160 68 L188 85" stroke="var(--nogo)"/>
<text x="50" y="218">Pin diameter d</text><path d="M177 210 L210 183" stroke="var(--ink)"/>
<text x="273" y="239">Hole diameter D</text><path d="M295 222 L259 188" stroke="var(--ink)"/>
<path d="M90 307 L370 307 L370 328 L90 328 Z" fill="url(#hatch)" stroke="var(--ink)"/>
<text x="92" y="357">Illustrative base restraint and reaction</text>
<rect x="565" y="50" width="40" height="255" fill="var(--soft)" stroke="var(--ink)" stroke-width="2"/>
<rect x="537" y="142" width="96" height="36" rx="3" fill="var(--line)" stroke="var(--ink)"/>
<path d="M565 330 L605 330" stroke="var(--ink)" marker-start="url(#arrow)" marker-end="url(#arrow)"/>
<text x="520" y="354">Total thickness t</text><text x="652" y="161">Pin</text>
<text x="528" y="26">Plate stack, side view</text>
</svg>
<figcaption>The public example geometry is represented by a round head of radius
{number('lug.outer_radius_mm')} mm, a hole of diameter {number('lug.hole_diameter_mm')} mm,
a pin of diameter {number('lug.pin_diameter_mm')} mm and a main plate
{number('lug.plate_thickness_mm')} mm thick. Cheek thickness per side is
{number('lug.cheek_plate_thickness_mm')} mm; total thickness is {number('total_thickness_mm')} mm.
Dimensions come from the <a href="#source-example">example input</a>.
The lower plate length, pin length and restraint depiction are illustrative; no fabrication drawing is available.
The dashed tear-out planes are conceptual, not to scale; the
calculation uses a loaded ligament of {number('ligament_mm')} mm from the hole to the loaded edge.
The sling direction is an assumed axial in-plane load. Contact offset, welds and base flexibility are not modelled.</figcaption></figure>'''


def render(result: dict, values: dict, chart: dict) -> str:
    """Write one coherent engineering narrative with calculated numerical fields."""
    def n(key):
        places = {
            "lug.plate_thickness_mm": 3,
            "lug.cheek_plate_thickness_mm": 3,
            "total_thickness_mm": 3,
            "load.num_slings": 0,
            "bearing_factor": 2,
            "tension_factor": 2,
            "shear_factor": 2,
        }.get(key, 1)
        return f'<span data-number="{key}">{values[key]:.{places}f}</span>'

    def row(label, key, unit, source="source-example"):
        return (f'<tr><td>{label}</td><td>{n(key)}</td><td>{unit}</td>'
                f'<td><a href="#{source}">{"Implementation" if source == "source-code" else "Example input"}</a></td></tr>')

    inputs = "\n".join(row(label, key, unit) for label, key, unit in [
        ("Main plate thickness", "lug.plate_thickness_mm", "mm"),
        ("Cheek thickness per side", "lug.cheek_plate_thickness_mm", "mm"),
        ("Pin diameter", "lug.pin_diameter_mm", "mm"),
        ("Hole diameter", "lug.hole_diameter_mm", "mm"),
        ("Outer radius from hole centre", "lug.outer_radius_mm", "mm"),
        ("Yield strength", "lug.yield_strength_mpa", "MPa"),
        ("Most-loaded leg static tension", "load.static_load_kN", "kN"),
        ("Dynamic amplification factor", "load.dynamic_amplification_factor", "dimensionless"),
        ("Entered skew factor", "load.skew_load_factor", "dimensionless"),
        ("Sling count", "load.num_slings", "legs"),
    ])
    results = "\n".join(
        f'<tr><td>{NAMES[c["name"]]}</td><td>{n(c["name"] + ".demand_mpa")}</td>'
        f'<td>{n(c["name"] + ".allowable_mpa")}</td><td>{n(c["name"] + ".utilization")}</td>'
        f'<td>{"The screen passes." if c["passes"] else "The screen fails."}</td>'
        '<td><a href="#source-code">Module output</a></td></tr>' for c in result["checks"])
    body = f'''
<header><div class="kicker">Lifting attachments · strength screening</div>
<h1>Lifting lug screening analysis</h1>
<p class="sub">Draft for maintainer review</p></header>
<nav aria-label="Report navigation"><a href="#summary">Executive summary</a> ·
<a href="#basis">Design data</a> · <a href="#example">Worked example</a> ·
<a href="#recommendations">Recommendations</a> · <a href="#appendix">Appendices</a></nav>

<section aria-labelledby="summary"><h2 id="summary">1 Executive Summary</h2>
<p>The lifting lug module is an analytical screen for a circular plate loaded through a pin.
Analysis is performed to determine the governing bearing, net-tension and shear tear-out demand;
key results are summarized below. The <a href="#source-example">public example</a> is assessed using the
<a href="#source-code">implemented strength factors</a>, whose design-code applicability remains NOT_EVALUATED.</p>
<ul><li>The example is held from design acceptance.
<ul><li>The shear tear-out utilization is {n('governing_percent')} % against the
{n('limit_percent')} % screening limit; the exceedance is {n('exceedance_percent')} % of the limit.</li>
<li>The bearing and net-tension screens pass; that outcome excludes welds, bending, fatigue and rigging adequacy.</li></ul></li>
<li>A restriction for the unchanged example geometry is available for engineering review.
<ul><li>The most-loaded leg static tension is restricted to {n('static_cap_kN')} kN for the same dynamic and skew factors.
This rounded-down value passes only the implemented screens.</li>
<li>Material certification, load basis, connection resistance and code applicability are required before design release.</li></ul></li></ul>
<h3>Revision history</h3>
<table><thead><tr><th>Revision</th><th>Date</th><th>What changed and why</th></tr></thead>
<tbody><tr><td>A</td><td>{REVISION}</td><td>The public example and load restriction are documented to expose the failed shear screen and unresolved design basis.</td></tr>
<tr><td>B</td><td>2026-10-22</td><td>Explicit text output, hand-calculation tests, conceptual geometry and field precision address reproducibility and interpretation.</td></tr></tbody></table>
</section>

<section aria-labelledby="introduction"><h2 id="introduction">2 Introduction</h2>
<p>A circular lifting lug is assessed under an already resolved in-plane sling tension.
The main workflow is covered from load entry to the governing strength result and summary output.</p>
<p>The public functions accept numerical geometry and material inputs; the configuration interface also handles
the skew-factor floor and writes a summary. The workflow is intended for preliminary screening.
The published example is repository-authored; it is not an independent textbook benchmark or a field measurement
(<a href="#source-guide">example guide</a>).</p>
<h3>Not covered</h3>
<p>The interpolated lug atlas, whole-rigging statics, crane motion, splash-zone loads and finite-element
padeye studies are outside this workflow. Vessel heading, a sea-state envelope and live forecast support are
NOT_EVALUATED because the module has no hydrodynamic response or weather inputs. No installability statement is made.</p>
</section>

<section aria-labelledby="controls"><h2 id="controls">3 Load Sensitivity</h2>
<p>The most-loaded leg tension is supplied after the sling-angle and centre-of-gravity effects are resolved.
Separate dynamic amplification and skew allowances are then applied, as described in the
<a href="#source-guide">example guide</a>.</p>
<p>The governing demand is shown against the module limit below. All geometry, material and load factors are held
at the public example values while static leg tension is varied. The chart is a strength-screen sensitivity,
not a sea-state or installation operating envelope.</p>
<figure><div id="load-chart"></div>
<p id="chart-readout" role="status" aria-live="polite"></p>
<label for="case-slider">Select static leg tension for component demands</label>
<input id="case-slider" type="range" min="0" max="{len(chart['cases']) - 1}" value="{next(i for i, c in enumerate(chart['cases']) if c['static_load_kN'] == values['load.static_load_kN'])}">
<figcaption>Utilization versus static leg tension. Hover or focus a point for case, component, demand, limit and units.
The dashed line is the {n('limit_percent')} % implementation limit. The example is selected initially.
The screen passes when all strengths pass and the entered skew factor satisfies its floor; otherwise the screen fails.</figcaption></figure>
<p>The same selected case is broken down below. Each bar represents one failure mode and the vertical line marks its screening limit.</p>
<div id="component-chart"></div>
</section>

<section aria-labelledby="basis"><h2 id="basis">4 Design Data and Basis</h2>
<p>The lug and pin are idealised as a concentric, in-plane connection with equal resistance on the ligaments.
The data are taken from the <a href="#source-example">public example input</a>; no material or fabrication record accompanies it.</p>
{schematic(values, n)}
<h3>Criteria and rationale</h3>
<p>The implemented bearing, tension and shear factors are respectively {n('bearing_factor')},
{n('tension_factor')} and {n('shear_factor')} times the supplied yield strength
(<a href="#source-code">implementation</a>). They are reproduced to explain the software result.
The repository calls them allowable-stress factors but identifies no edition, clause or table
of the American Institute of Steel Construction (AISC) design provisions for them.
Their suitability as design allowables is NOT_EVALUATED; no standard-compliance conclusion is drawn.</p>
<p>A utilization at or below {n('limit_percent')} % passes a strength screen by the module's comparison.
The entered skew factor is {n('load.skew_load_factor')}; the returned minimum is
{n('skew_load_factor_minimum')}, and the governing factor is {n('skew_load_factor')}
(<a href="#source-code">router output</a>). The repository attributes the leg-count floor to
<cite>Portable offshore units</cite>, April 2016, “Lift points – in-plane loads”, identified in the source register.
The clause text has not been independently examined for this analysis. The
<a href="#source-guide">example guide</a> records applicability restrictions and directs marine-operation
lifts to an independently assessed skew factor. Applying a numerical floor does not establish that scope is satisfied.</p>
<p>The assumed basis is axial in-plane loading with full participating plate thickness.
The dynamic amplification and yield strength are ASSUMED - to be confirmed.
The appendix data register identifies each evidence gap.</p>
</section>

<section aria-labelledby="method"><h2 id="method">5 Methodology and Equations</h2>
<p>The factored load is formed by multiplication, and nominal stresses are obtained from idealised resisting areas.
The following equations reproduce the <a href="#source-code">public module implementation</a>, rather than a complete padeye design method.</p>
<div class="eq">P = P<sub>s</sub> A<sub>d</sub> K; &nbsp; t = t<sub>p</sub> + 2t<sub>c</sub>;
&nbsp; r = D/2; &nbsp; ℓ = R − r</div>
<p>P is the factored sling load and P<sub>s</sub> is static tension in the most-loaded leg, both in kN.
A<sub>d</sub> is the dimensionless dynamic amplification factor. K is the larger of the entered skew factor
and the implemented leg-count minimum. The plate thickness t<sub>p</sub>, each cheek thickness t<sub>c</sub>,
total thickness t, hole diameter D, hole radius r, outer radius R and loaded ligament ℓ are in mm.
The multiplier on cheek thickness represents the cheek on each side of the main plate.</p>
<div class="eq">σ<sub>b</sub> = 1000P/(dt); &nbsp;
σ<sub>t</sub> = 1000P/(2tℓ); &nbsp; τ = 1000P/(2tℓ)</div>
<p>The pin diameter d is in mm. Bearing stress σ<sub>b</sub>, net-tension stress σ<sub>t</sub> and shear stress τ
are in MPa. The unit conversion is from kN to N; force in N divided by area in mm² gives MPa.
The net and tear-out expressions use the same idealised area, so their demands are equal in this implementation.</p>
<div class="eq">S<sub>b</sub> = a<sub>b</sub>F<sub>y</sub>; &nbsp;
S<sub>t</sub> = a<sub>t</sub>F<sub>y</sub>; &nbsp; S<sub>s</sub> = a<sub>s</sub>F<sub>y</sub>;
&nbsp; u = demand/allowable</div>
<p>F<sub>y</sub> is the entered yield strength in MPa; a<sub>b</sub>, a<sub>t</sub> and a<sub>s</sub> are the
implementation factors stated in the basis section. S<sub>b</sub>, S<sub>t</sub> and S<sub>s</sub> are
the resulting nominal stress limits in MPa. Utilization u is dimensionless and is displayed as a percentage.
The largest utilization governs. A below-minimum entered skew factor fails the overall screen even after
the governing floor has been used for the load.</p>
</section>

<section aria-labelledby="example"><h2 id="example">6 Worked Example and Results</h2>
<p>The documented public configuration is evaluated by calling the module's public configuration interface.
No example input or calculated stress is transcribed into the report.</p>
<p>The input static tension is {n('load.static_load_kN')} kN and the dynamic factor is
{n('load.dynamic_amplification_factor')}; with the governing skew factor of {n('skew_load_factor')},
the returned design load is {n('design_load_kN')} kN. The calculated total thickness is
{n('total_thickness_mm')} mm and the loaded ligament is {n('ligament_mm')} mm.
The geometry, yield input and factors are listed with their sources in the appendix.</p>
<table><thead><tr><th>Component</th><th>Demand (MPa)</th><th>Limit (MPa)</th><th>Utilization (%)</th><th>Result</th><th>Source</th></tr></thead>
<tbody>{results}</tbody></table>
<p>The shear tear-out screen fails at {n('governing_percent')} % utilization against the
{n('limit_percent')} % limit. Its exceedance is {n('exceedance_percent')} % of the limit.
The summary reports a failed screen, governed by shear tear-out. The outcome is a rejection of the example
under the implemented criteria, not a prediction of physical rupture or a complete design rejection.</p>
<p>With geometry and factors unchanged, the limiting static tension is calculated by dividing the example static
tension by the unrounded governing utilization and rounding down to the displayed precision.
At {n('static_cap_kN')} kN the public interface returns a passing screen. This restriction is conditional on the
implementation basis; attachment and operating acceptance remain NOT_EVALUATED.</p>
</section>

<section aria-labelledby="validation"><h2 id="validation">7 Validation Evidence</h2>
<p>Arithmetic and configuration behaviour are checked by the existing repository tests.
Physical validation of this circular-lug screen is not established by those checks.</p>
<p>The <a href="#source-tests">existing workflow tests</a> compare the example load, governing utilization
and nominal stresses with hand-calculated expressions. They also exercise missing and invalid skew factors,
unsupported sling counts, and below-minimum factors with otherwise passing strength results.
Those tests demonstrate agreement with the coded formulas and the intended input guards.</p>
<p>The <a href="#source-rebuild">report rebuild tests</a> compare displayed example fields with independent
hand-calculated literals and all plotted cases with hand arithmetic and fresh public-interface results.
The rounded-down restriction and the next load increment are checked for passing and failing screens respectively.
The committed page is rebuilt byte for byte with its change data, including a simulated CRLF platform.
No field-load measurements or destructive-test
results are supplied with the workflow example. The wider repository contains finite-element padeye material,
but no matched comparison to this example is established here; no numerical agreement with it is claimed.</p>
</section>

<section aria-labelledby="recommendations"><h2 id="recommendations">8 Key Findings, Limits and Recommendations</h2>
<p>The example is governed by the implemented shear tear-out limit, while evidence for design acceptance remains incomplete.
The following actions are tied to the <a href="#source-code">calculation</a> and the limitations in the
<a href="#source-guide">example guide</a>.</p>
<ul><li>Hold the example from release. Restrict the most-loaded leg static tension to
{n('static_cap_kN')} kN at the unchanged dynamic and skew factors for preliminary screening only.
Obtain a complete connection assessment before treating any passing screen as an operating permission.</li>
<li>If the entered tension is retained, assess an increased participating thickness or loaded ligament,
then repeat the strength, pin, weld and parent-plate checks. Cheek participation requires a load-transfer assessment.</li>
<li>Confirm the material, tolerances, effective thickness, extreme-centre-of-gravity tension and amplification basis.
Confirm the governing standard and edition, including skew scope, before selecting design criteria.</li>
<li>Provide separate checks for out-of-plane bending, pin bending and clearance effects, welds, parent structure,
fatigue and fracture. These resistances are not returned by the current module.</li>
<li>Validate positive finite geometry, force and material inputs before direct function use. The current low-level
functions do not comprehensively reject non-finite values, non-positive pin diameter or negative design force.</li></ul>
<p>A proposed next development slice is a single documented finite-input contract shared by the public functions
and configuration interface, followed by an independent circular-lug benchmark with an explicit design-code basis.
Until that evidence is available, design-code applicability and physical validation remain NOT_EVALUATED.</p>
</section>

<section aria-labelledby="references"><h2 id="references">9 References and Use</h2>
<p>The calculation basis is bounded by publicly inspectable repository records.
An implementation citation establishes what is computed; it does not establish an external standard's requirements.</p>
<h3>9.1 Inputs, outputs and interpretation</h3>
<p>The documented numerical load helper accepts static leg force, dynamic amplification and skew factor.
The numerical strength helper accepts design force, total thickness, pin and hole diameters, outer radius
and yield strength, and returns named demand, allowable, utilization and pass fields.
The configuration interface accepts main and cheek thicknesses separately, requires an explicit skew factor,
optionally enforces a sling-count floor, and returns the governing result and overall screening status.</p>
<p>The configuration interface writes a JavaScript Object Notation summary to the requested directory.
Its payload includes input and governing skew factors, the minimum and source, the below-minimum flag,
design load, total thickness, component checks and the governing utilization.
The returned summary path is added after writing, so that path is absent from the file itself.
The text label naming standards in the payload is not evidence of design compliance.</p>
<h3>9.2 Source register and reproducibility locators</h3>
<ul class="refs">
<li id="source-example">Public example input: <a href="../../../../examples/workflows/lifting-lug-design/input.yml">examples/workflows/lifting-lug-design/input.yml</a>.
This is the sole numerical example source; no external edition applies to this configuration.</li>
<li id="source-guide">Public example guide: <a href="../../../../examples/workflows/lifting-lug-design/README.md">examples/workflows/lifting-lug-design/README.md</a>,
“Lifting Lug / Padeye Strength Screening”, including its most-loaded-leg and skew-scope notes.</li>
<li id="source-code">Calculation implementation: <a href="../../../../src/digitalmodel/lifting_lug/workflow.py">src/digitalmodel/lifting_lug/workflow.py</a>,
functions <code>design_load_kn</code>, <code>lug_checks</code>, <code>router</code> and <code>_check</code>.
The strength factors are implementation constants; the AISC edition and clause are not established.</li>
<li id="source-skew">Shared skew basis: <a href="../../../../src/digitalmodel/orcaflex/installation_analysis.py">src/digitalmodel/orcaflex/installation_analysis.py</a>,
<code>_SKL_BY_LEG_COUNT</code> and <code>_SKL_REFERENCE</code>.
The repository cites DNVGL-ST-E273, <cite>Portable offshore units</cite>, April 2016, §3.5.5,
“Lift points – in-plane loads”, with scope notes at §1.4.3 and skew effects at §3.5.4.
The <a href="https://www.dnv.com/rules-standards/">publisher's rules and standards catalogue</a> was accessible
on 22 October 2026; this catalogue does not confirm the numerical clause content.</li>
<li id="source-tests">Existing arithmetic and guard evidence:
<a href="../../../../tests/workflows/test_lifting_lug_design.py">tests/workflows/test_lifting_lug_design.py</a>.
The atlas query in that file concerns interpolation and is not an independent strength validation.</li>
<li id="source-rebuild">Rebuild and numerical comparison:
<a href="../../../../tests/reporting/test_lifting_lug_module_report.py">tests/reporting/test_lifting_lug_module_report.py</a> and
<a href="build_report.py">build_report.py</a>. Rebuild with
<code>PYTHONPATH=src python docs/reports/modules/liftinglug/build_report.py --output-dir REPORT_DIRECTORY --work-dir CALCULATION_DIRECTORY</code>.
The calculation directory receives module summaries; neither directory is inferred from a user profile.</li>
<li id="source-style">Layout exemplar:
<a href="../../../api/cfd/turbulent-flat-plate-verification.html">docs/api/cfd/turbulent-flat-plate-verification.html</a>;
brand stylesheet: <a href="../../../api/_assets/brand.css">docs/api/_assets/brand.css</a>.
Both stylesheets are inlined for offline reading.</li></ul>
</section>

<section aria-labelledby="appendix"><h2 id="appendix">10 Appendices</h2>
<p>The numerical input record and unresolved assumptions are retained below.
The quality checks distinguish reproducible software evidence from missing design evidence.</p>
<h3>Numerical input record</h3>
<table><thead><tr><th>Input</th><th>Value</th><th>Unit</th><th>Source</th></tr></thead><tbody>{inputs}</tbody></table>
<h3>Data register</h3>
<table><thead><tr><th>Data or assumption</th><th>Status and consequence</th><th>Required evidence</th></tr></thead><tbody>
<tr><td>Example geometry, force, amplification and material</td><td>ASSUMED - to be confirmed. Published configuration values are used as supplied.</td><td>Approved geometry, material certificate and resolved lift-load basis.</td></tr>
<tr><td>Axial in-plane loading and participating plate stack</td><td>ASSUMED - to be confirmed. Nominal stress formulas omit eccentricity and load-transfer flexibility.</td><td>Connection arrangement and cheek/weld load-transfer calculation.</td></tr>
<tr><td>Strength-factor design basis</td><td>NOT_EVALUATED. The factors explain software behaviour only.</td><td>Governing standard, edition, clause and engineer-selected design criteria.</td></tr>
<tr><td>Skew-factor scope</td><td>NOT_EVALUATED. The numerical floor is applied; applicability is not established.</td><td>Rigging arrangement, tolerances, angle, load distribution and governing lifting-standard scope.</td></tr>
<tr><td>Pin, weld, parent structure, bending, fatigue and fracture</td><td>NOT_EVALUATED. A passing strength screen is insufficient for design acceptance.</td><td>Complete connection calculations and applicable acceptance criteria.</td></tr>
<tr><td>Field or independent benchmark evidence</td><td>NOT_EVALUATED. Arithmetic tests do not establish physical accuracy.</td><td>Matched published benchmark or controlled test with geometry, load and failure mode.</td></tr>
</tbody></table>
<h3>Quality pre-flight evidence</h3>
<p>The rebuild tests check numerical fields, every plotted load case, the rounded-down restriction, local source links,
the inline stylesheet, offline resource independence and embedded review controls.
Existing workflow tests cover formula agreement and skew input handling.
Engineering acceptance remains unevaluated for the data-register items above.</p>
<details id="change-panel"><summary>Engineering changes</summary><div id="change-content"></div></details>
<details id="comments"><summary>Comments</summary>
<p>Comments are kept in this page until saved. Select a report section, enter a comment and add it to the list.
Save the comments file beside this report. Where direct file saving is unavailable, move the downloaded file beside
the report. Import that file to continue reviewing. Unsaved comments are lost when the page is closed.</p>
<label for="comment-section">Report section</label><select id="comment-section"></select>
<label for="comment-text">Comment</label><textarea id="comment-text" rows="4"></textarea>
<button id="add-comment" type="button">Add comment</button>
<button id="save-comments" type="button">Save comments beside report</button>
<label for="import-comments">Import saved comments</label><input id="import-comments" type="file" accept=".json,application/json">
<p id="comment-status" role="status" aria-live="polite">No comments have been added.</p><ol id="comment-list"></ol>
</details>
</section>'''
    return f'''<!DOCTYPE html>
<html lang="en" data-theme="light"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Lifting lug screening analysis</title>
<style>{house_style()}</style>
<style>
svg{{max-width:100%;height:auto;color:var(--ink)}}svg text{{font:14px sans-serif;fill:var(--ink)}}
input[type=range],textarea{{width:100%}}label{{display:block;margin-top:12px}}
button,select,textarea,input[type=file]{{font:inherit;margin:6px 6px 6px 0;padding:6px}}
details{{margin:18px 0;padding:12px;border:1px solid var(--line);border-radius:6px}}
summary{{cursor:pointer}}.chart-point:focus{{outline:2px solid var(--accent)}}
@media print{{details{{display:block}}button,input,textarea,select{{display:none}}}}
</style></head><body class="hs-doc"><main class="wrap">
{body}
</main>
<script id="chart-data" type="application/json">{json.dumps(chart, ensure_ascii=False, separators=(',', ':'))}</script>
<script id="changes-data" type="application/json">{json.dumps(CHANGES, ensure_ascii=False, separators=(',', ':'))}</script>
<script>{INTERACTION}</script>
</body></html>
'''


INTERACTION = r'''
"use strict";
const data = JSON.parse(document.getElementById("chart-data").textContent);
const slider = document.getElementById("case-slider");
const svgNS = "http://www.w3.org/2000/svg";
function element(name, attrs, text) {
  const node = document.createElementNS(svgNS, name);
  Object.entries(attrs).forEach(([key, value]) => node.setAttribute(key, value));
  if (text !== undefined) node.textContent = text;
  return node;
}
const maxLoad = Math.max(...data.cases.map(c => c.static_load_kN));
const maxUse = Math.max(data.limit_percent, ...data.cases.flatMap(c => c.checks.map(k => k.utilization_percent))) * 1.1;
const colors = ["var(--brand)", "var(--accent)", "var(--nogo)"];
const x = load => 65 + load / maxLoad * 680;
const y = use => 315 - use / maxUse * 260;
const chart = element("svg", {viewBox: "0 0 820 400", role: "img", "aria-label": "Component utilization against static leg tension"});
chart.append(element("path", {d: "M65 45 V315 H755", fill: "none", stroke: "currentColor"}));
chart.append(element("line", {x1: 65, x2: 755, y1: y(data.limit_percent), y2: y(data.limit_percent), stroke: "var(--ink)", "stroke-dasharray": "6 4"}));
chart.append(element("text", {x: 66, y: 26}, "Utilization (%)"));
chart.append(element("text", {x: 530, y: 350}, "Static leg tension (kN)"));
for (const fraction of [0, 0.5, 1]) {
  chart.append(element("text", {x: x(maxLoad * fraction), y: 335}, (maxLoad * fraction).toFixed(1)));
}
for (const use of [0, data.limit_percent]) {
  chart.append(element("text", {x: 8, y: y(use) + 5}, use.toFixed(1)));
}
function tooltip(c, k) {
  return "Case: static leg tension " + c.static_load_kN.toFixed(1) + " kN; component: " + data.names[k.component] +
    "; demand: " + k.demand_mpa.toFixed(1) + " MPa; limit: " + k.allowable_mpa.toFixed(1) +
    " MPa; utilization: " + k.utilization_percent.toFixed(1) + " %; screening limit: " + data.limit_percent.toFixed(1) + " %.";
}
data.cases[0].checks.forEach((check, component) => {
  const points = data.cases.map(c => x(c.static_load_kN) + "," + y(c.checks[component].utilization_percent)).join(" ");
  chart.append(element("polyline", {points, fill: "none", stroke: colors[component], "stroke-width": 2}));
  chart.append(element("text", {x: 65 + component * 240, y: 382, style: "fill:" + colors[component]}, data.names[check.component]));
  data.cases.forEach((c, index) => {
    const k = c.checks[component];
    const point = element("circle", {cx: x(c.static_load_kN), cy: y(k.utilization_percent), r: 4,
      fill: colors[component], tabindex: 0, class: "chart-point", "aria-label": tooltip(c, k)});
    point.append(element("title", {}, tooltip(c, k)));
    const select = () => { slider.value = index; showCase(); };
    point.addEventListener("mouseenter", select);
    point.addEventListener("focus", select);
    point.addEventListener("click", select);
    chart.append(point);
  });
});
document.getElementById("load-chart").append(chart);
function showCase() {
  const c = data.cases[Number(slider.value)];
  const governing = c.checks.reduce((a, b) => a.utilization_percent >= b.utilization_percent ? a : b);
  document.getElementById("chart-readout").textContent = tooltip(c, governing) +
    (c.status === "pass" ? " The screen passes." : " The screen fails.");
  const plot = element("svg", {viewBox: "0 0 820 210", role: "img", "aria-label": "Selected case component utilization"});
  const bx = use => 210 + use / maxUse * 500;
  plot.append(element("line", {x1: bx(data.limit_percent), x2: bx(data.limit_percent), y1: 10, y2: 165,
    stroke: "currentColor", "stroke-dasharray": "6 4"}));
  c.checks.forEach((k, i) => {
    plot.append(element("text", {x: 8, y: 40 + i * 50}, data.names[k.component]));
    const rect = element("rect", {x: 210, y: 22 + i * 50, width: bx(k.utilization_percent) - 210,
      height: 24, fill: colors[i], tabindex: 0, "aria-label": tooltip(c, k)});
    rect.append(element("title", {}, tooltip(c, k))); plot.append(rect);
    plot.append(element("text", {x: bx(k.utilization_percent) + 8, y: 40 + i * 50}, k.utilization_percent.toFixed(1) + " %"));
  });
  plot.append(element("text", {x: 210, y: 195}, "Utilization (%); dashed line = " + data.limit_percent.toFixed(1) + " % limit"));
  document.getElementById("component-chart").replaceChildren(plot);
}
slider.addEventListener("input", showCase); showCase();

const changes = JSON.parse(document.getElementById("changes-data").textContent);
for (const revision of changes.revisions) {
  const p = document.createElement("p");
  p.textContent = "Revision " + revision.revision + " · " + revision.date + ": " + revision.changes.join(" ");
  document.getElementById("change-content").append(p);
}
const sections = [...document.querySelectorAll("h2[id]")];
const sectionSelect = document.getElementById("comment-section");
for (const section of sections) {
  const option = document.createElement("option"); option.value = section.id; option.textContent = section.textContent;
  sectionSelect.append(option);
}
let comments = [];
const status = document.getElementById("comment-status");
function renderComments() {
  const list = document.getElementById("comment-list"); list.replaceChildren();
  for (const comment of comments) {
    const item = document.createElement("li");
    item.textContent = document.getElementById(comment.section).textContent + ": " + comment.text;
    list.append(item);
  }
}
document.getElementById("add-comment").addEventListener("click", () => {
  const field = document.getElementById("comment-text");
  if (!field.value.trim()) { status.textContent = "Enter a comment before adding it."; return; }
  comments.push({section: sectionSelect.value, text: field.value.trim()});
  renderComments(); field.value = ""; status.textContent = "Comments have changed. Save the comments file before closing.";
});
document.getElementById("save-comments").addEventListener("click", async () => {
  const payload = JSON.stringify({document: changes.document, revision: changes.revisions.at(-1).revision, comments}, null, 2) + "\n";
  try {
    if (window.showSaveFilePicker) {
      const handle = await window.showSaveFilePicker({suggestedName: "lifting-lug-analysis.comments.json",
        types: [{description: "Report comments", accept: {"application/json": [".json"]}}]});
      const writable = await handle.createWritable(); await writable.write(payload); await writable.close();
      status.textContent = "Comments have been saved to the selected file.";
    } else {
      const url = URL.createObjectURL(new Blob([payload], {type: "application/json"}));
      const link = document.createElement("a"); link.href = url; link.download = "lifting-lug-analysis.comments.json";
      link.click(); setTimeout(() => URL.revokeObjectURL(url), 1000);
      status.textContent = "Comments have been downloaded. Keep the file beside the report.";
    }
  } catch (error) { status.textContent = "Comments were not saved. The file selection was cancelled or access was unavailable."; }
});
document.getElementById("import-comments").addEventListener("change", async event => {
  const file = event.target.files[0]; if (!file) return;
  try {
    const loaded = JSON.parse(await file.text());
    const valid = loaded.document === changes.document && loaded.revision === changes.revisions.at(-1).revision &&
      Array.isArray(loaded.comments) && loaded.comments.every(c =>
        c && typeof c.text === "string" && sections.some(s => s.id === c.section));
    if (!valid) throw new Error("Invalid comments file");
    if (comments.length && !window.confirm("Replace the current comments with the imported file?")) return;
    comments = loaded.comments; renderComments(); status.textContent = "Saved comments have been imported.";
  } catch (error) { status.textContent = "The selected file is not a comments file for this report revision."; }
});
'''


def build(output_dir: Path, work_dir: Path) -> None:
    """Compute the public example and write deterministic report deliverables."""
    cfg = yaml.safe_load(read_utf8_lf(EXAMPLE))
    result = evaluate(cfg, work_dir / "example")
    values = numerical_fields(result)
    chart = chart_cases(cfg, values, work_dir / "sensitivity")
    limited = copy.deepcopy(cfg)
    limited["lifting_lug"]["load"]["static_load_kN"] = values["static_cap_kN"]
    if evaluate(limited, work_dir / "restriction")["screening_status"] != "pass":
        raise ValueError("Calculated restriction does not pass the module screen")
    output_dir.mkdir(parents=True, exist_ok=True)
    (output_dir / "lifting-lug-analysis.html").write_text(
        render(result, values, chart), encoding="utf-8", newline="\n")
    (output_dir / "changes.json").write_text(
        json.dumps(CHANGES, indent=2, ensure_ascii=False) + "\n", encoding="utf-8", newline="\n")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", type=Path, required=True)
    parser.add_argument("--work-dir", type=Path, required=True)
    args = parser.parse_args()
    build(args.output_dir, args.work_dir)
