#!/usr/bin/env python3
# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Build the standalone materials report from public materials interfaces.

Use PYTHONPATH=src python scripts/generate_materials_report.py.
Only the report and its engineering change panel are written. The generator
requires no network, plotting package, database service or engineering solver.
"""

from __future__ import annotations

import argparse
import html
import json
import re
from pathlib import Path
from typing import Any, TypedDict

from digitalmodel.materials import LinePipe, line_pipe, smts_mpa
from digitalmodel.materials.database import load_database
from digitalmodel.materials.line_pipe import _RHO_KG_M3

ROOT = Path(__file__).resolve().parents[1]
DESTINATION = ROOT / "docs/reports/modules/materials"
REPORT_NAME = "materials-module-report.html"
DATE = "2026-10-21"
REVISION = "00"


class PipeExample(TypedDict):
    """Catalogue selection for the public worked example."""

    grade: str
    nps: float
    schedule: str


# Public example: Continental Steel, WSP40-800-322, Specifications table,
# undated web edition, accessed 2026-10-21. X52 is an independent grade
# selection from UNIASEN, API 5L Pipe Specification, 46th ed. (2018),
# Mechanical Requirement PSL1/PSL2, accessed 2026-10-21. No availability claim.
EXAMPLE: PipeExample = {"grade": "X52", "nps": 8.0, "schedule": "SCH 40"}
PUBLIC_REFERENCE = {"od_in": 8.625, "wt_in": 0.322, "id_in": 7.981,
                    "weight_lb_ft": 28.55}
# Existing test_plain_end_weight_matches_asme tolerance, not a design margin.
WEIGHT_TOLERANCE_PERCENT = 0.3
CHANGES: dict[str, Any] = {"document": "Materials module analysis", "revision": REVISION,
           "date": DATE, "changes": [{
               "section": "Whole document",
               "description": "First issue: line-pipe geometry, grade selection and bare-pipe mass are explained; evidence gaps and the basis required for design use are identified."
           }]}


def example_values() -> tuple[LinePipe, dict[str, float]]:
    """Read numerical example values through the public module interfaces."""
    pipe = line_pipe(**EXAMPLE)
    if pipe.grade.rho_kg_m3 != _RHO_KG_M3:
        raise ValueError("grade density differs from the fixed plain-end mass basis")
    values = {key: getattr(pipe, key) for key in (
        "nps", "od_in", "wt_in", "id_in", "od_mm", "wt_mm", "id_mm",
        "d_over_t", "metal_area_in2", "bore_area_in2", "weight_kg_m", "weight_lb_ft"
    )}
    values.update(
        volume_l_m=pipe.internal_volume_m3_per_m * 1000,
        smys_mpa=pipe.grade.smys_mpa,
        smts_psl1_mpa=smts_mpa(pipe.grade.name, psl="PSL1"),
        smts_psl2_mpa=smts_mpa(pipe.grade.name, psl="PSL2"),
        rho_kg_m3=pipe.grade.rho_kg_m3, wall_limit_mm=pipe.od_mm / 2,
    )
    return pipe, values


def quantity(values: dict[str, float], key: str, precision: str = ".1f") -> str:
    """Mark every displayed module quantity for independent rebuild checks."""
    return (f'<span data-quantity="{key}" data-format="{precision}">'
            f'{format(values[key], precision)}</span>')


def chart_data(pipe: LinePipe) -> dict[str, Any]:
    """Evaluate a hollow-section sensitivity with explicit wall inputs.

    These wall values are illustration sampling locations, not manufactured
    schedules or allowable values. The selected catalogue wall is included.
    """
    walls = sorted({0.125, 0.25, pipe.wt_in, 0.5, 1.0, 1.5, 2.0,
                    2.5, 3.0, 3.5, 4.0})
    points = []
    for wall in walls:
        sample = line_pipe(EXAMPLE["grade"], od_in=pipe.od_in, wt_in=wall)
        points.append({"wt_in": wall, "wall_mm": format(sample.wt_mm, ".3f"),
                       "bore_mm": format(sample.id_mm, ".1f"),
                       "selected": wall == pipe.wt_in})
    return {"points": points, "wall_limit_mm": format(pipe.od_mm / 2, ".3f"),
            "outside_mm": format(pipe.od_mm, ".1f")}


def plot(pipe: LinePipe) -> str:
    """Draw an accessible offline plot with a geometric, open upper bound."""
    data = chart_data(pipe)
    circles = []
    for index, point in enumerate(data["points"]):
        x = 65 + 585 * float(point["wall_mm"]) / (pipe.od_mm / 2)
        y = 250 - 195 * float(point["bore_mm"]) / pipe.od_mm
        colour = "--caution" if point["selected"] else "--go"
        tip = (f'Case: {"selected catalogue section" if point["selected"] else "geometry sensitivity"}; '
               f'component: pipe wall; wall demand: {point["wall_mm"]} mm; '
               f'open wall limit: {data["wall_limit_mm"]} mm; '
               f'bore diameter: {point["bore_mm"]} mm.')
        circles.append(f'<circle tabindex="0" data-point="{index}" cx="{x:.1f}" '
                       f'cy="{y:.1f}" r="5" fill="var({colour})" '
                       f'aria-label="{html.escape(tip)}"><title>{tip}</title></circle>')
    return f'''<figure id="bore-plot">
<svg viewBox="0 0 740 315" role="img" aria-labelledby="plot-title plot-description" style="width:100%">
<title id="plot-title">Bore diameter against wall thickness</title>
<desc id="plot-description">Outside diameter is fixed. The red line marks the excluded solid-section boundary. Hover or focus a point to read its dimensions.</desc>
<path d="M65 45V250H680" fill="none" stroke="var(--ink)"/>
<path d="M65 55L650 250" fill="none" stroke="var(--go)"/>
<path id="geometry-limit" d="M650 45V250" stroke="var(--nogo)" stroke-dasharray="6 4"/>
<circle cx="650" cy="250" r="6" fill="white" stroke="var(--nogo)"/>
<circle id="zero-wall-limit" cx="65" cy="55" r="6" fill="white" stroke="var(--nogo)"/>
<text x="70" y="31">Bore diameter (mm)</text>
<text x="240" y="303">Wall thickness (mm)</text>
<text x="45" y="269">0</text>
<text x="15" y="60">{pipe.od_mm:.1f}</text>
<text x="625" y="275">{pipe.od_mm / 2:.3f}</text>
<text x="410" y="40" fill="var(--nogo)">Hollow-section boundary is excluded.</text>
<text x="80" y="80" fill="var(--nogo)">Zero wall is excluded.</text>
{''.join(circles)}
</svg>
<label for="point-select">Select a section to read its dimensions</label>
<input id="point-select" type="range" min="0" max="{len(data['points']) - 1}" value="2" step="1" style="width:100%">
<p id="chart-readout" aria-live="polite"></p>
<figcaption>Figure 2. The bore closes at the hollow-section boundary; zero wall is also excluded. These geometric limits are not pressure, buckling or procurement limits. Green points retain a bore and positive wall. The amber point identifies the selected catalogue section. The red boundaries are excluded. Sources: <a href="#ref-method">method record</a> and <a href="#ref-public-dimensions">public dimensions</a>.</figcaption>
</figure><script id="chart-data" type="application/json">{json.dumps(data, ensure_ascii=False)}</script>'''


def schematic(pipe: LinePipe, values: dict[str, float]) -> str:
    """Scale the annulus to the computed diameter-to-wall relationship."""
    radius = 104 * pipe.id_in / pipe.od_in
    q = lambda key, precision=".1f": quantity(values, key, precision)
    return f'''<figure id="pipe-section">
<svg viewBox="0 0 720 295" role="img" aria-labelledby="section-title section-description" style="width:100%">
<title id="section-title">Dimensioned annular pipe cross-section</title>
<desc id="section-description">A concentric bare steel pipe is shown at its calculated wall-to-diameter scale. No coating, contents or applied load is included.</desc>
<circle cx="215" cy="148" r="104" fill="var(--faint)" stroke="var(--ink)"/>
<circle cx="215" cy="148" r="{radius:.1f}" fill="var(--panel)" stroke="var(--ink)"/>
<path d="M111 148H319M215 44V252" stroke="var(--muted)" stroke-dasharray="5 5"/>
<path d="M111 42V25H319V42M111 25L119 21M111 25L119 29M319 25L311 21M319 25L311 29" fill="none" stroke="var(--ink)"/>
<text x="143" y="16">Outside diameter D</text>
<path d="M305 115L395 75H640M285 175L395 203H640" fill="none" stroke="var(--ink)"/>
<text x="400" y="65">Steel wall, thickness t</text>
<text x="400" y="225">Empty bore, diameter d</text>
<text x="110" y="283">Concentric circular section; bare pipe only</text>
</svg>
<figcaption>Figure 1. Outside diameter D = {q('od_mm')} mm, wall t = {q('wt_mm', '.3f')} mm and bore d = {q('id_mm')} mm are computed for the selected public dimensions. The material wall and empty bore are shown to scale; dimensions govern. Source: <a href="#ref-method">method record</a>.</figcaption>
</figure>'''


def table(values: dict[str, float]) -> str:
    """Render all worked-example quantities directly from module values."""
    rows = [
        ("Outside diameter", "od_in", ".3f", "in", "Public dimensions and catalogue lookup"),
        ("Wall thickness", "wt_in", ".3f", "in", "Public dimensions and catalogue lookup"),
        ("Inside diameter", "id_in", ".3f", "in", "Circular-section identity"),
        ("Outside diameter", "od_mm", ".3f", "mm", "Module length conversion"),
        ("Wall thickness", "wt_mm", ".3f", "mm", "Module length conversion"),
        ("Inside diameter", "id_mm", ".3f", "mm", "Module length conversion"),
        ("Diameter / wall", "d_over_t", ".1f", "dimensionless", "Circular-section identity"),
        ("Metal area", "metal_area_in2", ".1f", "in²", "Annular-section identity"),
        ("Bore area", "bore_area_in2", ".1f", "in²", "Circular-section identity"),
        ("Plain-end mass per length", "weight_kg_m", ".1f", "kg/m", "Module calculation; assumed density"),
        ("Plain-end mass per length", "weight_lb_ft", ".1f", "lb/ft", "Module mass conversion"),
        ("Internal volume per length", "volume_l_m", ".1f", "L/m", "Module bore volume; litre conversion"),
        ("Specified minimum yield strength", "smys_mpa", ".1f", "MPa", "Public grade table; repository grade record"),
        ("Specified minimum tensile strength, product specification level 1", "smts_psl1_mpa", ".1f", "MPa", "Public grade table; explicit level selection"),
        ("Specified minimum tensile strength, product specification level 2", "smts_psl2_mpa", ".1f", "MPa", "Public grade table; explicit level selection"),
    ]
    tables = []
    for label, subset in (("A.1a. Nominal geometry", rows[:6]),
                          ("A.1b. Derived quantities and strengths", rows[6:])):
        cells = ''.join(f'<tr><td>{name}</td><td>{quantity(values, key, fmt)}</td>'
                        f'<td>{unit}</td><td>{source}</td></tr>'
                        for name, key, fmt, unit, source in subset)
        tables.append(f'''<table><thead><tr><th scope="col">Quantity</th><th scope="col">Value</th><th scope="col">Unit</th><th scope="col">Source</th></tr></thead>
<tbody>{cells}</tbody><caption style="caption-side:bottom;text-align:left">Table {label}. The source names resolve to the records in section 9.2. Full precision is retained in the module calls; displayed values are rounded.</caption></table>''')
    return ''.join(tables)


def render_report() -> str:
    """Render the report without external resources or uncontrolled writes."""
    database = load_database()
    if not database.source_digest_matches:
        raise ValueError("materials source digest differs from pinned provenance")
    pipe, values = example_values()
    for key in ("od_in", "wt_in", "id_in"):
        if getattr(pipe, key) != PUBLIC_REFERENCE[key]:
            raise ValueError(f"public example dimension changed: {key}")
    q = lambda key, precision=".1f": quantity(values, key, precision)
    difference = 100 * (pipe.weight_lb_ft - PUBLIC_REFERENCE["weight_lb_ft"]) / PUBLIC_REFERENCE["weight_lb_ft"]
    if abs(difference) > WEIGHT_TOLERANCE_PERCENT:
        raise ValueError("public plain-end mass check exceeds the existing test tolerance")
    brand = (ROOT / "docs/api/_assets/brand.css").read_text(encoding="utf-8")
    exemplar = (ROOT / "docs/api/cfd/turbulent-flat-plate-verification.html").read_text(encoding="utf-8")
    style_match = re.search(r"<style>(.*?)</style>", exemplar, re.S)
    if style_match is None:
        raise ValueError("the exemplar house stylesheet is missing")
    layout = style_match[1]
    body = f'''
<header><p class="kicker">Materials · engineering analysis</p>
<h1>Materials module analysis</h1><p class="sub">Grade selection, line-pipe geometry and bare-pipe mass</p>
<p>Draft for client review</p><p>Revision {REVISION} · {DATE}</p></header>
<nav aria-label="Contents"><p><a href="#executive-summary">Summary</a> · <a href="#basis">Basis</a> · <a href="#worked-example">Example</a> · <a href="#validation">Validation</a> · <a href="#recommendations">Recommendations</a> · <a href="#appendix">Appendix</a> · <a href="#comments">Comments</a></p></nav>
<section id="executive-summary"><h2>1. Executive Summary</h2>
<p>The materials module is a library of steel-grade properties and line-pipe section calculations. Analysis is performed to determine the information available for preliminary material selection and bare-pipe quantity estimates; key results are summarized below.</p>
<ul><li>Use the documented pipe-size example to obtain section geometry and nominal bare-pipe mass.
<ul><li>The computed plain-end mass is {q('weight_kg_m')} kg/m. Its difference from the public reference is {difference:+.1f} %, within the existing numerical-check tolerance of ±{WEIGHT_TOLERANCE_PERCENT:.1f} %; this criterion checks a tabulated mass, not structural adequacy.</li></ul></li>
<li>Keep the strength basis separate from the dimensional catalogue.
<ul><li>The selected grade supplies specified minima; procurement qualification and allowable operating pressure are NOT_EVALUATED.</li></ul></li>
<li>Obtain the missing design basis before release for engineering use.
<ul><li>Confirm dimensional editions, material certification, tolerances, corrosion allowance and operating loads. The displayed geometric boundary only requires a positive hollow bore.</li></ul></li></ul>
</section>
<div id="revision-history"><h3>Revision history</h3>
<table><thead><tr><th scope="col">Revision</th><th scope="col">Date</th><th scope="col">What changed and why</th></tr></thead><tbody><tr><td>{REVISION}</td><td>{DATE}</td><td>{CHANGES['changes'][0]['description']}</td></tr></tbody><caption style="caption-side:bottom;text-align:left">Document revisions, newest first.</caption></table></div>
<section id="introduction"><h2>2. Introduction</h2>
<p>Steel-grade lookup and pipe-section calculations are examined as a connected workflow. A public catalogue example is used to show how material identity, dimensions and derived quantities remain distinguishable.</p>
<p>The module stores minimum strengths for line-pipe, hull, structural, austenitic stainless and duplex steels. It also exposes an indexed material database, lower-bound toughness estimates and monotonic true stress–strain curves. Only the grade-to-line-pipe workflow is evaluated numerically in this document.</p>
<p>Nominal pipe size is a catalogue label rather than a measured outside diameter. Product specification level is the quality and testing level attached to a line-pipe strength query; the pipe-section object does not itself certify either level.</p>
</section>
<section id="basis"><h2>3. Basis and criteria</h2>
<p>The calculation basis is separated into published nominal geometry, a selected grade and an assumed density. Numerical agreement is judged against existing repository assertions; design acceptance is not assigned.</p>
<h3>3.1 Source and material basis</h3>
<p>The selected dimensions are the public standard-wall pipe example in the <a href="#ref-public-dimensions">dimensional reference</a>. The independently selected X52 grade is listed in the <a href="#ref-public-grade">public grade reference</a>. Their combination demonstrates the module interface; availability of that exact product is not established.</p>
<p>The grade record cites API Specification 5L, Line Pipe, edition 46 (2018), clauses 9.3.1 and 9.3.2, tensile-property tables for product specification levels 1 and 2. The dimensional catalogue cites ASME B36.10M, Welded and Seamless Wrought Steel Pipe; its edition is not recorded. Licensed source tables have not been independently requalified by this analysis.</p>
<p>The mass calculation uses a density of {q('rho_kg_m3', '.0f')} kg/m³ from the <a href="#ref-method">method record</a>: ASSUMED - to be confirmed for the ordered material. The line-pipe mass implementation uses a fixed carbon-steel density; a different grade density is not automatically substituted.</p>
<h3>3.2 Acceptance criteria</h3>
<p>The catalogue dimensions are required to match the public dimensional entries. The mass comparison uses the existing <a href="#ref-tests">plain-end mass assertion</a>, with a relative tolerance equivalent to ±{WEIGHT_TOLERANCE_PERCENT:.1f} %. This is a rounding and numerical-consistency criterion, not an operating limit. The geometry requires D &gt; 0, t &gt; 0 and t &lt; D/2, where D is outside diameter and t is wall thickness. Equality at the upper boundary removes the hollow bore and is rejected.</p>
<p>Pressure, external collapse, local buckling, fracture, fatigue, weld qualification and corrosion-life criteria are NOT_EVALUATED. No loads, manufacturing tolerances, inspection measurements or material certificates are supplied for this example.</p>
</section>
<section id="interface"><h2>4. Inputs and outputs</h2>
<p>A grade and a complete diameter-and-wall specification are required by the public pipe factory. The returned immutable section contains nominal dimensions, a grade record and derived cross-section properties.</p>
<h3>4.1 Required inputs</h3>
<p>A canonical grade name or recognised alias is supplied. Diameter is entered as nominal pipe size or explicit outside diameter in inches. Wall thickness is entered as a catalogue schedule with nominal pipe size, or as an explicit value in inches. Providing both alternatives for either dimension is rejected; missing or unlisted combinations are rejected.</p>
<p>Grade strengths are stored in megapascals. A tensile-strength query selects the product specification level explicitly; the default query selects level 2. An absent level is rejected rather than replaced with another level. Exact grade identities are retained: a bare line-pipe grade and a hull grade with the same letter remain different materials.</p>
<h3>4.2 Returned information</h3>
<p>Outputs include inside diameter, diameter-to-wall ratio, metal area, bore area, plain-end mass per length and bore volume per length. Metric dimensions and customary mass units are available. The text product reference is an identifier assembled by the module; it is not a certificate of compliance.</p>
<p>The indexed database adds source citations, nullable edition fields and explicit missing links to other material datasets. A matching source fingerprint establishes agreement with the inspected repository data only. Original-standard qualification and purchase-order suitability are separate requirements.</p>
</section>
<section id="method"><h2>5. Theory and equations</h2>
<p>A concentric, uniform circular section is used to calculate the geometry. Mass per length is obtained by multiplying metal area by the fixed implementation density.</p>
<h3>5.1 Section relationships</h3>
<p>Inside diameter is denoted d, metal area A<sub>m</sub>, bore area A<sub>b</sub>, density ρ, mass per length m′ and bore volume per length V′. All lengths in each equation use the same unit.</p>
<div class="eq">d = D − 2t<br>A<sub>m</sub> = π(D² − d²)/4 = πt(D − t)<br>A<sub>b</sub> = πd²/4<br>m′ = ρA<sub>m</sub><br>V′ = A<sub>b</sub><br>Diameter-to-wall ratio = D/t</div>
<p>These are annular and circular area identities, implemented by the <a href="#ref-method">method record</a>; π is the circle constant, and the factors follow directly from the radius-to-diameter relation. Areas are converted to square metres before multiplying by density. Bore volume is reported in litres per metre using the metric volume relation in the <a href="#ref-units">unit reference</a>.</p>
<h3>5.2 Representation</h3>
<p>The cross-section is shown with the calculated wall-to-diameter relationship. No pressure boundary condition, finite-element mesh, coating, contents, weld reinforcement, end fittings or corrosion loss is modelled.</p>
{schematic(pipe, values)}
</section>
<section id="worked-example"><h2>6. Worked example and results</h2>
<p>The public example is computed by calling the module with nominal pipe size {q('nps', '.0f')}, schedule 40 and grade X52. The size and schedule reproduce the dimensional reference; the grade is selected independently from the public grade reference.</p>
<p>The module returns outside diameter {q('od_in', '.3f')} in and wall thickness {q('wt_in', '.3f')} in, with inside diameter {q('id_in', '.3f')} in. Plain-end mass is {q('weight_lb_ft')} lb/ft, equivalent to {q('weight_kg_m')} kg/m. These are nominal bare-pipe quantities.</p>
<p>Every numerical result is generated from the module call; the complete output is given in <a href="#example-table">Tables A.1a and A.1b</a>. The figure varies explicit wall thickness at the same outside diameter to illustrate the hollow-section constraint. Sensitivity sections are mathematical examples, not available catalogue products.</p>
<p>The upper wall boundary is {q('wall_limit_mm', '.3f')} mm. A wall at or above that value is outside the hollow-section domain; no bore capacity is reported there.</p>
{plot(pipe)}
</section>
<section id="validation"><h2>7. Validation evidence</h2>
<p>Repository validation is assessed from the shipped tests and public references. The evidence supports numerical consistency for the exercised cases, with the qualification limits stated below.</p>
<p>The line-pipe tests compare nominal outside diameters and published plain-end masses, check area-to-mass consistency, exercise schedule aliases and reject ordinary missing, conflicting and invalid geometry. The grade tests check published minima, distinguish hull and line-pipe aliases and preserve legacy conversions. The database tests check identity, nullable edition metadata and source fingerprint behaviour.</p>
<p>The selected outside diameter, wall and bore match the public dimensional table. The computed mass differs from the public mass by {difference:+.1f} %, meeting the existing ±{WEIGHT_TOLERANCE_PERCENT:.1f} % numerical criterion. The public dimensional page is internally mixed in its product-standard labelling; it is used only as a dimensional and mass reference. The selected grade minima agree with the public supplier table; this is not independent certification against the licensed specification.</p>
<p>The existing toughness tests exercise equation anchors, material-class restrictions and input units; the stress–strain tests exercise yield, hardening and curve-table relationships. These test records do not establish material-specific fracture performance. No measured pipe, mill certificate, laboratory curve or field benchmark is held for this example. Historical validation notes are retained as supporting records, but current source and test assertions govern this report.</p>
</section>
<section id="recommendations"><h2>8. Limits and recommendations</h2>
<p>Use is limited to nominal material lookup and circular-section calculations within the stated geometry. Engineering release remains conditional on a supplied design basis and qualified material data.</p>
<h3>8.1 Recommended actions</h3>
<p>For preliminary quantities, retain the bare-pipe basis and identify the density as assumed. Before procurement, confirm the dimensional-standard edition, product level, delivery condition and mill certification. Before pressure or structural assessment, supply tolerances, corrosion allowance, temperature, pressure and other loads to the applicable design assessment. This follows the missing inputs and unqualified fields identified in the <a href="#data-register">data register</a> and <a href="#ref-gap-record">material qualification record</a>.</p>
<p>Reject non-finite dimensions before calling the pipe factory; its comparison-only checks do not establish rejection of every non-finite value. Restrict this catalogue workflow to its intended line-pipe grade family: the factory also accepts other material records, yet its generated product reference retains the line-pipe label and its mass uses the fixed density. Source: <a href="#ref-method">method record</a>. A positive hollow bore alone does not establish a manufacturable or structurally adequate section.</p>
<p>Use canonical grade and explicit product-level queries for new work. Legacy adapters intentionally preserve earlier conversion conventions; equality with the canonical minimum is not established for every adapter. Review the adapter basis before combining its outputs with canonical values.</p>
<h3>8.2 Not covered</h3>
<p>Hull and general structural-grade selection, stainless and duplex product suitability, fracture-toughness estimation, monotonic stress–strain curve generation, fatigue-curve associations and structural-shape associations are not evaluated. Temperature-dependent strength reduction is not applied by the grade lookup. Toughness and hardening require their own measured or qualified inputs; no such inputs are inferred from grade identity.</p>
</section>
<section id="references"><h2>9. References and evidence</h2>
<p>Public references and repository records are identified separately. Web references were inspected on {DATE}; an absent edition remains an evidence gap.</p>
<h3>9.1 Source qualification</h3>
<p>Manufacturer and supplier publications are used as openly accessible numerical references, not as substitutes for the applicable licensed standard or purchase specification. The dimensional page is undated; no dimensional-standard edition is assigned. Repository records describe implementation and test coverage, not original-document approval.</p>
<h3>9.2 Source register and reproducibility</h3>
<ol class="refs">
<li id="ref-public-dimensions"><a href="https://continentalsteel.com/item/pipe-and-tubing/seamless-and-welded-pipe/wsp-800-322">Continental Steel, Seamless and Welded Pipe, item WSP40-800-322</a>, Specifications: Pipe Size, O.D., Thickness, I.D. and Weight (Plain End). Undated web edition. Nominal dimensions and mass comparator only.</li>
<li id="ref-public-grade"><a href="https://uniasen.com/standards/api-5l/">UNIASEN, API 5L Pipe Specification</a>, Specification and Mechanical Requirement PSL1/PSL2, L360/X52 rows. The page names edition 46 (2018). The repository cites API 5L, Line Pipe, clauses 9.3.1/Table 6 and 9.3.2/Table 7. Supplier transcription; original-table qualification remains pending.</li>
<li id="ref-method">Repository implementation: <code>src/digitalmodel/materials/line_pipe.py</code>, <code>LinePipe</code> geometry, mass and volume properties, density constant and <code>line_pipe</code>; <code>src/digitalmodel/materials/grades.py</code>, <code>get</code> and <code>smts_mpa</code>. Current repository edition. Geometry identities, conversions, grade records and legacy views.</li>
<li id="ref-tests">Repository tests: <code>tests/materials/test_line_pipe.py</code>, <code>test_plain_end_weight_matches_asme</code> and geometry assertions; <code>test_grade_matrix.py</code>, <code>test_audit_regressions.py</code> and <code>test_psl1_example.py</code>, grade assertions and executable public example. Current repository edition. Historical supporting records: <code>docs/domains/materials/line-pipe-validation-2026-06-28.md</code> and <code>grade-matrix-validation-2026-06-28.md</code>; their older counts and scope descriptions are not adopted as current results.</li>
<li id="ref-gap-record">Repository qualification record: <code>docs/domains/materials/database-gaps.html</code>; <code>src/digitalmodel/materials/database.py</code>, <code>load_database</code>; <code>database_schema.py</code>; <code>tests/materials/test_database.py</code>. Current repository edition. Nullable editions, missing dataset associations and source-fingerprint limitations.</li>
<li id="ref-units">Repository unit definitions: <code>line_pipe.py</code>, inch-to-metre and kilogram-per-metre conversions. <a href="https://www.bipm.org/en/publications/si-brochure">BIPM, The International System of Units, edition 9 (2019), version 4.01 (June 2026), Table 8, litre</a>: a litre is a cubic decimetre. The multiplier used for litres per metre follows from that definition. π and integer factors in the area identities are mathematical, not empirical coefficients.</li>
<li id="ref-report">Example call: <code>from digitalmodel.materials import line_pipe; pipe = line_pipe("X52", nps=8.0, schedule="SCH 40")</code>. Rebuild: <code>PYTHONPATH=src python scripts/generate_materials_report.py</code>. Verification: <code>PYTHONPATH=src python -m pytest tests/materials</code>. New assertions: <code>tests/materials/test_materials_report.py</code>. Report layout is reused from <code>docs/api/cfd/turbulent-flat-plate-verification.html</code> and <code>docs/api/_assets/brand.css</code>; both stylesheets are inlined. The required <code>hs-doc</code> body class identifies the document surface.</li>
</ol>
</section>
<section id="appendix"><h2>10. Appendices</h2>
<p>Detailed output, missing-data disposition and calculation checks are recorded below. Numerical entries are generated at issue from the public interface and are rounded only for display.</p>
<h3 id="example-table">10.1 Computed output and data register</h3>
{table(values)}
<table id="data-register"><thead><tr><th scope="col">Data</th><th scope="col">Readiness</th><th scope="col">Disposition and source</th></tr></thead><tbody>
<tr><td>Nominal dimensions</td><td>Public reference is available.</td><td>Geometry matches the public dimensional entries; original dimensional edition remains unestablished. Source: dimensional reference in section 9.2.</td></tr>
<tr><td>Grade minima</td><td>Supplier transcription and repository values agree.</td><td>Original-table qualification and ordered-product level remain required. Source: public grade reference in section 9.2.</td></tr>
<tr><td>Density</td><td>ASSUMED - to be confirmed</td><td>{q('rho_kg_m3', '.0f')} kg/m³ is the fixed implementation basis. Source: method record in section 9.2. Replace or qualify the mass basis for a different material.</td></tr>
<tr><td>Manufacturing tolerance, corrosion allowance and coating</td><td>NOT_EVALUATED</td><td>No measured or specified values are supplied; obtain the procurement and inspection basis.</td></tr>
<tr><td>Operating loads and temperature properties</td><td>NOT_EVALUATED</td><td>No pressure or structural acceptance calculation is performed; supply the design case before assessment.</td></tr>
<tr><td>Material certification and delivery condition</td><td>NOT_EVALUATED</td><td>Grade identity does not establish a supplied product's test results.</td></tr>
<tr><td>Fracture, hardening and fatigue inputs</td><td>NOT_EVALUATED</td><td>These sub-areas are not evaluated; no default is inferred from the selected grade.</td></tr>
</tbody><caption style="caption-side:bottom;text-align:left">Table A.2. Data register and conditions of use. NOT_EVALUATED means evidence or calculation is absent; it is not a zero result.</caption></table>
<h3>10.2 Quality checks and document comments</h3>
<p>The existing materials tests pass at the time of issue. Rebuild checks compare every marked example quantity and chart point with fresh module calls, require deterministic output and detect a changed module result. Style inlining, local links, source-fingerprint mismatch rejection and visible wording are checked. These checks establish reproducibility of this numerical example; browser interaction and material-source qualification are separate evidence.</p>
<p>The public reference mass is {PUBLIC_REFERENCE['weight_lb_ft']:.2f} lb/ft. Its signed percentage difference is calculated from unrounded values as 100 × (computed − reference) / reference. The nominal geometry uses the stated positive hollow-bore domain. No numerical result outside that domain is returned.</p>
<details id="change-panel"><summary>Engineering changes</summary><div id="changes-list"></div></details>
<div id="comments"><p>Comments are attached to this revision. Select a section and enter a comment. Save the comment file in the same folder as this report. A local browser copy supports recovery; it does not replace a saved file.</p>
<label for="comment-section">Section</label><select id="comment-section">{''.join(f'<option value="{n}">{n}</option>' for n in range(1, 11))}</select>
<label for="comment-text">Comment</label><textarea id="comment-text" rows="4" style="width:100%" maxlength="4000"></textarea>
<button id="add-comment" type="button">Add comment</button> <button id="save-comments" type="button">Save comments beside report</button>
<label for="import-comments">Load saved comments</label><input id="import-comments" type="file" accept="application/json,.json">
<p id="comment-status" role="status"></p><ol id="comment-list"></ol></div>
</section>'''
    return f'''<!DOCTYPE html>
<html lang="en" data-theme="light"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Materials module analysis</title><style>{brand}</style><style>{layout}</style>
</head><body class="hs-doc"><main class="wrap">{body}</main>
<script id="changes-data" type="application/json">{json.dumps(CHANGES, ensure_ascii=False)}</script>
<script>{INTERACTION}</script></body></html>
'''


INTERACTION = r'''
"use strict";
(() => {
  const data = JSON.parse(document.getElementById("chart-data").textContent);
  function showPoint(index) {
    const point = data.points[index];
    document.getElementById("point-select").value = index;
    document.getElementById("chart-readout").textContent =
      `Case: ${point.selected ? "selected catalogue section" : "geometry sensitivity"}; component: pipe wall; wall demand: ${point.wall_mm} mm; open wall limit: ${data.wall_limit_mm} mm; bore diameter: ${point.bore_mm} mm.`;
  }
  document.querySelectorAll("[data-point]").forEach(point => {
    ["mouseenter", "focus", "click"].forEach(event =>
      point.addEventListener(event, () => showPoint(Number(point.dataset.point))));
  });
  document.getElementById("point-select").addEventListener("input", event => showPoint(Number(event.target.value)));
  showPoint(data.points.findIndex(point => point.selected));
  const changes = JSON.parse(document.getElementById("changes-data").textContent);
  changes.changes.forEach(change => {
    const line = document.createElement("p");
    line.textContent = `${change.section}: ${change.description}`;
    document.getElementById("changes-list").appendChild(line);
  });
  const key = "materials-module-report-comments-" + changes.revision;
  const status = document.getElementById("comment-status");
  let comments = [], handle = null;
  function validate(value) {
    if (!value || value.document !== changes.document || value.revision !== changes.revision ||
        !Array.isArray(value.comments) || value.comments.length > 500) throw new Error("The comment file does not match this document and revision.");
    return value.comments.map(item => {
      if (!item || !Number.isInteger(item.section) || item.section < 1 || item.section > 10 ||
          typeof item.text !== "string" || !item.text.trim() || item.text.length > 4000)
        throw new Error("A comment has an invalid section or text.");
      return {section: item.section, text: item.text};
    });
  }
  function envelope() { return {document: changes.document, revision: changes.revision, comments}; }
  function display() {
    const list = document.getElementById("comment-list");
    list.replaceChildren();
    comments.forEach(item => {
      const li = document.createElement("li");
      li.textContent = `Section ${item.section}: ${item.text}`;
      list.appendChild(li);
    });
  }
  function recover() {
    try {
      const saved = localStorage.getItem(key);
      if (saved) comments = validate(JSON.parse(saved));
    } catch (_) { status.textContent = "Local comment recovery is unavailable. Save a comment file for retention."; }
    display();
  }
  function remember() {
    try { localStorage.setItem(key, JSON.stringify(envelope())); }
    catch (_) { status.textContent = "Browser storage is unavailable. Save the comment file for retention."; }
  }
  document.getElementById("add-comment").addEventListener("click", () => {
    const input = document.getElementById("comment-text");
    if (!input.value.trim()) { status.textContent = "Enter comment text before adding it."; return; }
    if (comments.length >= 500) { status.textContent = "The comment limit is reached. Save this file before starting a separate record."; return; }
    comments.push({section: Number(document.getElementById("comment-section").value), text: input.value.trim()});
    input.value = "";
    status.textContent = "Comment added. Save the comment file for retention.";
    remember(); display();
  });
  document.getElementById("save-comments").addEventListener("click", async () => {
    const filename = "materials-module-report.comments.json";
    const text = JSON.stringify(envelope(), null, 2) + "\n";
    try {
      if (typeof window.showSaveFilePicker === "function") {
        if (!handle) handle = await window.showSaveFilePicker({suggestedName: filename,
          types: [{description: "Document comments", accept: {"application/json": [".json"]}}]});
        const writable = await handle.createWritable();
        await writable.write(text); await writable.close();
        status.textContent = "Comments saved to the selected file. Keep that file beside the report.";
      } else {
        const url = URL.createObjectURL(new Blob([text], {type: "application/json"}));
        const link = document.createElement("a");
        link.href = url; link.download = filename; link.click();
        setTimeout(() => URL.revokeObjectURL(url), 1000);
        status.textContent = "Comment download requested. Place the downloaded file beside the report.";
      }
    } catch (error) {
      status.textContent = error.name === "AbortError" ? "Save cancelled; comments remain in this page." : "Comments were not saved. Check file access and try again.";
    }
  });
  document.getElementById("import-comments").addEventListener("change", async event => {
    try {
      const file = event.target.files[0];
      if (!file) return;
      if (file.size > 2000000) throw new Error("The comment file exceeds the size limit.");
      comments = validate(JSON.parse(await file.text()));
      remember(); display(); status.textContent = "Saved comments loaded for this revision.";
    } catch (error) { status.textContent = "Comments were not loaded: " + error.message; }
  });
  recover();
})();
'''


def build(destination: Path = DESTINATION) -> None:
    """Write the report and identical embedded/sidecar engineering changes."""
    document = render_report()
    destination = Path(destination)
    destination.mkdir(parents=True, exist_ok=True)
    (destination / REPORT_NAME).write_text(document, encoding="utf-8", newline="\n")
    (destination / "changes.json").write_text(
        json.dumps(CHANGES, ensure_ascii=False, indent=2) + "\n",
        encoding="utf-8", newline="\n",
    )


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", type=Path, default=DESTINATION)
    build(parser.parse_args().output_dir)
