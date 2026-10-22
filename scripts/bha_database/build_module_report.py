# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Build the self-contained bottom hole assembly database analysis.

Run from the repository root with PYTHONPATH=src. The public module supplies
every example quantity. Only report formatting and geometry are added here.
"""

from __future__ import annotations

import argparse
import ast
from html import escape
import json
from pathlib import Path
import re

from digitalmodel.bha_database import load_database
from digitalmodel.bha_database.iso_table14 import audit_table14

ROOT = Path(__file__).resolve().parents[2]
OUTPUT = ROOT / "docs/reports/modules/bhadatabase/report.html"
EXAMPLE = "SYNTHETIC-BUILDING"
LABELS = {
    "bit": "Bit",
    "stabilizer": "Near-bit stabilizer",
    "mwd": "Measurement while drilling tool",
    "drill_collar": "Drill collar",
    "hwdp": "Heavy-weight drill pipe",
}
TEST_FILES = ("test_database.py", "test_iso_table14.py", "test_table14_audit.py")
TEST_LABELS = ("Catalogue and assembly behaviour", "Nominal-table regression",
               "Comparison metadata")


def collect():
    """Call the documented interface and collect inputs and calculated values."""
    db = load_database()
    assembly = db.assembly(EXAMPLE)
    summary = db.summary(EXAMPLE)
    sections = db.sections(EXAMPLE)
    numbers = {}

    def add(key, value, unit, digits, source="catalogue"):
        numbers[key] = dict(value=value, unit=unit, digits=digits, source=source)

    for key in ("total_length_m", "max_od_m", "air_weight_N", "buoyed_weight_N"):
        q = summary[key]
        add(key, q["value"], q["unit"], 4 if q["unit"] == "m" else 1)
    add("available_wob_N", summary["r7"]["available_wob_N"]["value"], "N", 1)
    add("hole_m", assembly.hole_size.to("m"), "m", 4)
    add("planned_wob_N", assembly.planned_wob.to("N"), "N", 1)
    add("planned_wob_lbf", assembly.planned_wob.to("lbf"), "lbf", 0)
    add("mud_density", assembly.mud_weight_basis.to("lb/gallon"), "lb/US gal", 1)
    add("material_density", assembly.material_density.to("lb/gallon"), "lb/US gal", 1)
    add("margin", assembly.wob_margin.to("percent"), "%", 1)
    add("hole_in", assembly.hole_size.to("inch"), "in", 0)
    add("blade_centre_m", sections[1]["blade_centre_from_bit"]["value"], "m", 4)
    add("blade_length_m", db.components[assembly.items[1].component_id].blade_length.to("m"), "m", 4)
    add("blade_offset_m", db.components[assembly.items[1].component_id].blade_bottom_to_component_bottom.to("m"), "m", 4)
    bit = db.components[assembly.items[0].component_id]
    add("foot_m", bit.length.to("m") / bit.length.to("ft"), "m/ft", 4, "units")
    add("inch_m", bit.od.to("m") / bit.od.to("inch"), "m/in", 4, "units")
    add("lbf_N", assembly.planned_wob.to("N") / assembly.planned_wob.to("lbf"), "N/lbf", 6, "units")
    labels = []
    for index, (item, section) in enumerate(zip(assembly.items, sections)):
        component = db.components[item.component_id]
        labels.append(LABELS[component.type])
        for name in ("length", "od", "id", "distance_from_bit"):
            q = section[name]
            add(f"section_{index}_{name}", q["value"], q["unit"], 4)
        add(f"section_{index}_length_ft", (item.length_override or component.length).to("ft"), "ft", 0)
        add(f"section_{index}_count", item.count, "joints", 0)
        add(f"section_{index}_weight", component.weight_per_length.to("lbf/ft"), "lbf/ft", 0)
        envelope = max(q.to("m") for q in (component.od, component.od_max, component.blade_od or component.od))
        add(f"section_{index}_envelope", envelope, "m", 4)
    inventory = []
    for filename in TEST_FILES:
        tree = ast.parse((ROOT / "tests/bha_database" / filename).read_text(encoding="utf-8"))
        count = sum(isinstance(n, ast.FunctionDef) and n.name.startswith("test_") for n in ast.walk(tree))
        inventory.append(dict(file=filename, functions=count))
        add(filename.removesuffix(".py") + "_count", count, "functions", 0, "tests")
    add("test_functions", sum(row["functions"] for row in inventory), "functions", 0, "tests")
    add("assembly_count", len(db.assemblies), "assemblies", 0)
    add("component_count", len(db.components), "components", 0)
    add("source_count", len(db.sources), "sources", 0)
    audit = audit_table14()
    for key, value in audit["counts"].items():
        add("audit_" + key, value, "cells", 0, "comparison")
    return dict(numbers=numbers, labels=labels, state=summary["r7"]["state"],
                audit=audit["counts"], inventory=inventory)


def number(record, key, tag="span"):
    """Render only the declared display precision; retain a checkable key."""
    q = record["numbers"][key]
    value = f'{q["value"]:.{q["digits"]}f}'
    return f'<{tag} data-number="{key}">{value}</{tag}>'


def reference(key):
    """Link a claim to its reference entry without inline file locators."""
    return f'<a href="#{key}">[source]</a>'


def schematic(record):
    """Draw the assumed cylindrical geometry with a separate length scale."""
    n = lambda key: number(record, key, "tspan")
    values = record["numbers"]
    total = values["total_length_m"]["value"]
    hole = values["hole_m"]["value"]
    xscale, yscale, centre = 600, 480 / total, 175
    pieces = []
    # Axial distances increase upward from the bit. Cross-sections use the
    # catalogue diameters; cutter and blade silhouettes are symbolic.
    for index, label in enumerate(record["labels"]):
        z = values[f"section_{index}_distance_from_bit"]["value"]
        length = values[f"section_{index}_length"]["value"]
        od = values[f"section_{index}_od"]["value"]
        bore = values[f"section_{index}_id"]["value"]
        y = 540 - (z + length) * yscale
        height, half, inside = length * yscale, od * xscale / 2, bore * xscale / 2
        left, right = centre - half, centre + half
        if index == 0:
            path = (f"M{left:.2f},{y:.2f} H{right:.2f} L{right-7:.2f},{y+height:.2f} "
                    f"L{centre:.2f},{y+height-2:.2f} L{left+7:.2f},{y+height:.2f} Z")
        else:
            path = (f"M{left:.2f},{y:.2f} Q{centre:.2f},{y-5:.2f} {right:.2f},{y:.2f} "
                    f"V{y+height:.2f} Q{centre:.2f},{y+height+5:.2f} {left:.2f},{y+height:.2f} Z")
        pieces.append(f'<path d="{path}" fill="var(--soft)" stroke="var(--ink)"/>')
        pieces.append(f'<path d="M{centre-inside:.2f},{y:.2f} V{y+height:.2f} M{centre+inside:.2f},{y:.2f} V{y+height:.2f}" stroke="var(--mut)" fill="none"/>')
        if index == 1:
            blade_half = values[f"section_{index}_envelope"]["value"] * xscale / 2
            blade_height = values["blade_length_m"]["value"] * yscale
            bottom = 540 - z * yscale
            for sign in (-1, 1):
                x0, x1 = centre + sign * half, centre + sign * blade_half
                pieces.append(f'<path d="M{x0:.2f},{bottom:.2f} L{x1:.2f},{bottom-3:.2f} V{bottom-blade_height+3:.2f} L{x0:.2f},{bottom-blade_height:.2f} Z" fill="var(--mut)"/>')
        # Small components receive offset callouts to keep labels distinct.
        label_y = [575, 510, 460, 330, 130][index]
        pieces.append(f'<path d="M{right:.2f},{y+height/2:.2f} L330,{label_y-5} H350" fill="none" stroke="var(--line)"/>')
        pieces.append(f'<text x="360" y="{label_y}">{escape(label)}: {n(f"section_{index}_length")} m</text>')
    half_hole = hole * xscale / 2
    return f'''<figure id="assembly-figure">
<svg viewBox="0 0 780 620" role="img" aria-labelledby="assembly-title assembly-desc">
<title id="assembly-title">Synthetic building assembly, bit-up order</title>
<desc id="assembly-desc">Bit, near-bit stabilizer, measurement tool, drill collar and heavy-weight drill pipe are shown inside a nominal hole. Axial length and diameter use separate scales. No contact reactions are calculated.</desc>
<path d="M{centre-half_hole:.2f},60 V548 M{centre+half_hole:.2f},60 V548" stroke="var(--red)" stroke-dasharray="5 4" fill="none"/>
{''.join(pieces)}
<text x="55" y="30">Hole diameter {n('hole_m')} m</text>
<text x="355" y="45">Top: model ends at the pipe; no top load is solved.</text>
<text x="355" y="80">Fluid density {n('mud_density')} lb/US gal</text>
<text x="355" y="100">Weight acts downward; buoyancy acts upward.</text>
<text x="30" y="600">Bit datum; distance increases upward. Total length {n('total_length_m')} m.</text>
</svg>
<figcaption>Figure A. Assumed catalogue dimensions are drawn from the module sections. Cutter and blade outlines are symbolic; the drawing is not a manufacturing detail. The bit-up distance is measured along the assembly, not as vertical elevation. {reference('catalogue')}</figcaption>
</figure>'''


def diameter_chart(record):
    """Plot a focusable, hoverable diameter envelope against its hole limit."""
    n = lambda key: number(record, key, "tspan")
    limit = record["numbers"]["hole_m"]["value"]
    scale = 300 / limit
    limit_y = 390 - limit * scale
    bars = []
    for index, label in enumerate(record["labels"]):
        key = f"section_{index}_envelope"
        value = record["numbers"][key]["value"]
        height = value * scale
        x = 130 + index * 130
        label_short = ("Bit", "Stabilizer", "Measurement tool", "Collar", "Heavy-weight pipe")[index]
        bars.append(f'''<g class="datum" tabindex="0" role="img" aria-label="{escape(label)}; inspect diameter and limit" data-component="{escape(label)}">
<title>Case: synthetic building. Component: {escape(label)}. Demand: {value:.4f} m. Limit: {limit:.4f} m. Nominal diameter check only.</title>
<rect x="{x}" y="{390-height:.2f}" width="65" height="{height:.2f}" fill="var(--brand)"/>
<text x="{x-8}" y="430" font-size="12">{label_short}</text>
<text x="{x-8}" y="{380-height:.2f}">{n(key)} m</text></g>''')
    return f'''<figure id="diameter-chart">
<svg viewBox="0 0 830 470" role="group" aria-labelledby="diameter-title">
<title id="diameter-title">Maximum component diameter against hole-size limit</title>
<text x="25" y="30">Maximum outside diameter (m)</text>
<path d="M90,60 V390 H810" stroke="var(--ink)" fill="none"/>
<path d="M90,{limit_y:.2f} H810" stroke="var(--red)" stroke-dasharray="6 4"/>
<text x="400" y="60">Hole-size limit: {n('hole_m')} m</text>
{''.join(bars)}
</svg>
<p id="diameter-detail" aria-live="polite">Point to or focus a component to read its demand, limit and units.</p>
<figcaption>Figure B. Only the nominal diameter check is represented. The bit equals the hole-size limit; wear, hydraulic clearance, contact and dimensional tolerances are not evaluated. {reference('catalogue')} {reference('implementation')}</figcaption>
</figure>'''


def render(record):
    """Write the complete engineering narrative with module-derived numbers."""
    n = lambda key: number(record, key)
    exemplar = (ROOT / "docs/api/cfd/turbulent-flat-plate-verification.html").read_text(encoding="utf-8")
    style = re.search(r"<style>.*?</style>", exemplar, re.S).group()
    brand = (ROOT / "docs/api/_assets/brand.css").read_text(encoding="utf-8")
    changes = (OUTPUT.parent / "changes.json").read_text(encoding="utf-8").strip()
    input_rows, output_rows = [], []
    for index, label in enumerate(record["labels"]):
        input_rows.append(f'''<tr><td>{escape(label)}</td><td>{n(f'section_{index}_count')}</td>
<td>{n(f'section_{index}_length_ft')}</td><td>{n(f'section_{index}_od')}</td>
<td>{n(f'section_{index}_id')}</td><td>{n(f'section_{index}_weight')}</td><td><a href="#catalogue">Assumed catalogue</a></td></tr>''')
        output_rows.append(f'''<tr><td>{escape(label)}</td><td>{n(f'section_{index}_length')}</td>
<td>{n(f'section_{index}_distance_from_bit')}</td><td>{n(f'section_{index}_envelope')}</td><td><a href="#input-table">Table A inputs</a>; <a href="#implementation">module sections</a></td></tr>''')
    inventory_rows = "".join(
        f'<tr><td>{label}</td><td>{n(row["file"].removesuffix(".py") + "_count")}</td><td><a href="#tests">Test inventory</a></td></tr>'
        for row, label in zip(record["inventory"], TEST_LABELS))
    return f'''<!doctype html>
<html lang="en" data-theme="light">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<meta name="description" content="Bottom hole assembly database analysis with a synthetic calculated example and stated limits of use.">
<title>Bottom hole assembly database analysis</title>
<style>{brand}</style>
{style}
<style>
svg{{width:100%;height:auto;font-family:inherit;fill:var(--ink)}}
.datum:focus rect,.datum:hover rect{{fill:var(--accent);stroke:var(--ink);stroke-width:2}}
textarea{{display:block;width:100%;min-height:6em;font:inherit}}
button,select,input{{font:inherit;margin:.3em;padding:.3em}}
pre{{white-space:pre-wrap;overflow-wrap:anywhere}}
details{{margin:1em 0}} caption{{caption-side:bottom;text-align:left;color:var(--mut)}}
</style>
</head>
<body class="hs-doc"><main class="wrap">
<header><div class="kicker">Engineering module analysis</div>
<h1>Bottom hole assembly database analysis</h1>
<p class="sub">Draft for maintainer review</p>
<p>Revision 1 · 22 October 2026</p></header>
<nav aria-label="Report sections"><p>
<a href="#executive">Executive summary</a> · <a href="#introduction">Introduction</a> ·
<a href="#workflow">Workflow</a> · <a href="#basis">Basis</a> ·
<a href="#method">Methodology</a> · <a href="#example">Worked example</a> ·
<a href="#validation">Validation</a> · <a href="#limits">Limits and recommendations</a> ·
<a href="#references">References</a> · <a href="#appendix">Appendix</a></p></nav>

<section id="executive"><h2>1. Executive summary</h2>
<p>The bottom hole assembly database is an illustrative catalogue and assembly checker for drilling components. Analysis is performed to determine assembly dimensions, static weights and the scope of the available checks; key results are summarized below.</p>
<ul><li>The documented synthetic building assembly is available for calculation teaching and interface trials.
<ul><li>The computed length is {n('total_length_m')} m and the maximum outside diameter is {n('max_od_m')} m, equal to the assumed hole-size limit. Nominal equality does not establish running clearance. {reference('catalogue')} {reference('implementation')}</li></ul></li>
<li>The weight-on-bit advisory is within its assumed limit for this example.
<ul><li>The planned load is {n('planned_wob_N')} N and the available load is {n('available_wob_N')} N after the assumed margin. This is not an equipment rating or a drilling acceptance result. {reference('catalogue')} {reference('implementation')}</li></ul></li>
<li>Equipment selection remains outside the evidence supplied.
<ul><li>The separate nominal-table comparison is NOT_EVALUATED. Published-source comparison records and qualified operating criteria are required before design use. {reference('comparison')}</li></ul></li></ul>
<h3>Revision history</h3>
<table><thead><tr><th>Revision</th><th>Date</th><th>What changed and why</th></tr></thead>
<tbody><tr><td>1</td><td>22 October 2026</td><td>The documented synthetic calculation and evidence limits are assembled to distinguish catalogue checks from equipment qualification.</td></tr></tbody></table>
</section>

<section id="introduction"><h2>2. Introduction</h2>
<p>A drilling bottom hole assembly is represented as an ordered sequence from the bit upward. The database is assessed as a data-handling and elementary mechanics module, with the documented public example used as the numerical basis.</p>
<p>The catalogue contains {n('assembly_count')} synthetic assemblies, {n('component_count')} components and {n('source_count')} source records. The rotary building workflow is covered in full; the pendulum and motor examples are identified under “Not covered”. None of the quantities establishes a manufactured component property. {reference('catalogue')}</p>
<p>Inputs are stored with units and provenance. Outputs preserve the calculation formula and resolving source keys, allowing an engineer to distinguish a supplied quantity from a computed quantity. A digest checks file consistency; it does not authenticate the source. {reference('implementation')}</p>
</section>

<section id="workflow"><h2>3. Main workflow and physical representation</h2>
<p>The building assembly is loaded, checked and summarized through the documented interface. The physical sequence is shown with nominal cylindrical geometry and an explicit bit datum.</p>
<p>The bit is followed by a near-bit stabilizer, a measurement while drilling tool, a drill collar and heavy-weight drill pipe. The final pipe marks the end of this modelled assembly. The names “building” and “pendulum” classify the examples; directional drilling response is not calculated. Density units below use the United States (US) liquid gallon. {reference('catalogue')}</p>
{schematic(record)}
<p>Source-checkout use is illustrated in the reference appendix. An installed caller supplies an explicit catalogue directory containing the expected data files. Loading rejects invalid geometry, unknown component references and incompatible internal connections before the summary is obtained. {reference('implementation')}</p>
<p>The input set contains source records, component records and ordered assembly records. Each physical quantity requires a value, unit, source identity, basis and the original representation and date; a stored computed quantity also requires a formula. Joint counts and positions are integers. A per-joint length override is optional. The loader requires the fixed catalogue and manifest files and rejects unexpected files, aliases, duplicate keys and symlinks. {reference('implementation')}</p>
<p>The summary returns total length, maximum outside diameter, air weight, buoyed weight when density is available, an advisory state and warnings. Sections return length, diameters and distance from the bit, with the first blade centre where supplied. Calculated quantity records contain their unit, formula, computed basis and source references. Invalid input raises a database error; a successful call still requires inspection of the advisory state. {reference('implementation')}</p>
</section>

<section id="basis"><h2>4. Design basis and criteria</h2>
<p>All physical inputs in this example are treated as ASSUMED - to be confirmed. The catalogue assumptions are used unchanged so that the published teaching example is calculated without assigning an equipment rating.</p>
<p>The hole diameter is {n('hole_in')} in; the fluid and material densities are {n('mud_density')} and {n('material_density')} lb/US gal. The advisory margin is {n('margin')} %, selected in the catalogue rather than prescribed by a standard. The planned weight on bit is {n('planned_wob_lbf')} lbf. The appendix records the complete component inputs and their basis. {reference('catalogue')}</p>
<p>The nominal bit diameter is required to match the hole diameter, and the body, maximum and blade diameters are limited by the hole diameter. These are consistency criteria with the rationale that the represented assembly fits the nominal modelled hole. They are not clearance criteria. A relative numerical tolerance is used by the implementation for equality; no physical tolerance is assigned. {reference('implementation')}</p>
<p>Collars precede heavy-weight pipe and drill pipe in bit-up order, while intervening tools are permitted. Internal interfaces require matching labels and opposite pin/box genders. External end interfaces and actual thread ratings are not qualified. The weight-on-bit check is advisory because contact, inclination and neutral-point position are absent. {reference('implementation')}</p>
</section>

<section id="method"><h2>5. Methodology and equations</h2>
<p>Section lengths and distances are accumulated from the selected joint lengths and counts. Static buoyancy is applied through the uniform-material approximation, and the assumed margin is applied only to weight below the first heavy-weight pipe.</p>
<p>The geometric definitions use section length ℓ<sub>i</sub>, joint count n<sub>i</sub>, selected per-joint length L<sub>i</sub>, total length L and bit-up coordinate z<sub>i</sub>. Indices i and j identify sections counted upward from the bit. Counts are dimensionless and lengths are in metres. Scalar lengths with common units are added by ordinary algebra. These expressions define the aggregation performed by the module, rather than an empirical drilling relation. {reference('geometry')} {reference('implementation')}</p>
<div class="eq">ℓ<sub>i</sub> = n<sub>i</sub>L<sub>i</sub>; L = Σ ℓ<sub>i</sub>; z<sub>i</sub> = Σ<sub>j&lt;i</sub> ℓ<sub>j</sub>.</div>
<p>The first blade centre is located by adding the section coordinate, the supplied lower blade offset and half the supplied blade length. A repeated section reports the blade on its first joint only. These are coordinate definitions, with common-unit scalar addition as the basis. {reference('geometry')} {reference('implementation')}</p>
<p>With weight per length w<sub>i</sub> in newtons per metre, the air weight W<sub>a</sub> is the sum of the section weights. The buoyant force B equals the displaced-fluid weight. The uniform-material reduction follows by dividing B = ρ<sub>f</sub>gV by W<sub>a</sub> = ρ<sub>s</sub>gV, where ρ<sub>f</sub> is fluid density, ρ<sub>s</sub> is material density, g is gravitational acceleration and V is material displacement volume. The density ratio is dimensionless; the same units are required for both densities. {reference('geometry')} {reference('mechanics')}</p>
<div class="eq">W<sub>a</sub> = Σ ℓ<sub>i</sub>w<sub>i</sub>; W<sub>b</sub> = W<sub>a</sub> − B = W<sub>a</sub>(1 − ρ<sub>f</sub>/ρ<sub>s</sub>).</div>
<p>The buoyed weight W<sub>b</sub> is in newtons. This reduction represents a uniform material with hydrostatic fluid access consistent with material displacement. Sealed cavities, differing internal fluid density and mixed component displacement require a separate pressure and displaced-volume model. No pressure field is solved here. {reference('mechanics')} {reference('implementation')}</p>
<p>The available advisory load W<sub>lim</sub> is defined from the below-pipe air weight W<sub>below</sub> and the dimensionless assumed margin m. The planned load W<sub>p</sub> is compared with this value. The expression is a catalogue policy, not a sourced design-code safety factor; the physical buoyancy term has the basis given above. {reference('catalogue')} {reference('mechanics')}</p>
<div class="eq">W<sub>lim</sub> = W<sub>below</sub>(1 − ρ<sub>f</sub>/ρ<sub>s</sub>)(1 − m); advisory within assumed limit when W<sub>p</sub> ≤ W<sub>lim</sub>.</div>
<p>Heavy-weight pipe and all sections above it are excluded from W<sub>below</sub>. When no heavy-weight pipe exists, the whole modelled assembly is included. Missing density, planned load or margin produces NOT_EVALUATED. Total buoyed weight still includes the entire assembly when density is supplied. {reference('implementation')}</p>
<p>Unit conversion uses the module unit registry. The factors are {n('foot_m')} m/ft, {n('inch_m')} m/in and {n('lbf_N')} N/lbf at the displayed precision; the public units reference provides the conversion basis. No separate gravity value is selected by this report. {reference('units')}</p>
</section>

<section id="example"><h2>6. Worked example</h2>
<p>The documented synthetic building assembly is calculated by calling the module. Inputs, returned values and plotted diameters are generated from those calls, with the component detail placed in the appendix.</p>
<p>The returned length is {n('total_length_m')} m. Air weight is {n('air_weight_N')} N and buoyed weight is {n('buoyed_weight_N')} N. The near-bit blade centre is {n('blade_centre_m')} m from the bit datum. {reference('catalogue')} {reference('implementation')}</p>
<p>The planned load of {n('planned_wob_N')} N is below the returned advisory limit of {n('available_wob_N')} N after the assumed margin of {n('margin')} %. The returned state means that the assumed static comparison is met; buckling, contact, fatigue and equipment strength remain NOT_EVALUATED. {reference('catalogue')} {reference('implementation')}</p>
{diameter_chart(record)}
<p>Legend: the blue bars show calculated nominal diameter demand. The red dashed line shows the nominal hole-size limit. The advisory load is within its assumed limit. Equipment qualification is NOT_EVALUATED. Pointing to or focusing a bar displays the case, component, demand, limit and units.</p>
</section>

<section id="validation"><h2>7. Validation evidence</h2>
<p>The repository evidence is separated into calculation checks, input-rejection checks and source-comparison metadata. No field measurement or equipment-performance benchmark is supplied in the module evidence set.</p>
<p>The existing suite contains {n('test_functions')} test functions before parameter expansion. It covers synthetic loading, repeated-joint and override calculations, blade positions, unit round trips, geometry and connection rejection, provenance, bounded reads and restricted-record refusal. The function inventory is listed in the appendix; the count describes source inspection rather than an execution result. {reference('tests')}</p>
<p>The nominal-table tests pin the supplied transcription and exercise comparison metadata. The audit reports {n('audit_total')} cells: {n('audit_recorded')} with comparison records, {n('audit_pending')} pending and {n('audit_unspecified')} without a comparison status. The overall source-comparison status is NOT_EVALUATED. Counts describe repository metadata, not the contents of an authenticated standard. No nominal-table dimension or coefficient is reproduced here. {reference('comparison')}</p>
<p>The report tests add literal hand-calculation anchors for geometry and static weights, compare displayed example values against fresh module calls and rebuild the page. These checks establish consistency of this example and its presentation. They do not establish physical calibration or qualification for drilling. {reference('report-tests')}</p>
</section>

<section id="limits"><h2>8. Key findings, limits and recommendations</h2>
<p>Use is limited to illustrative catalogue assembly and elementary static calculations under the stated assumptions. The required next evidence is identified separately for data, source comparison and engineering acceptance.</p>
<h3>Recommendations</h3>
<p>For catalogue evaluation, retain the supplied assumptions and inspect both the returned state and its warnings. If the planned load exceeds the returned advisory value, reduce the illustrative planned load or revise the modelled below-pipe weight and recalculate; neither action establishes an operating limit. Missing inputs are supplied before interpreting the advisory. This response follows the documented advisory definition. {reference('implementation')}</p>
<p>For engineering selection, replace synthetic geometry, weights and densities with reviewed equipment data and establish a displacement model for the actual fluid boundaries. The uniform-material buoyancy reduction is restricted to its stated physical assumptions. A qualified load, clearance, strength and operating basis is required before acceptance is determined. {reference('mechanics')} {reference('catalogue')}</p>
<p>For the nominal reference table, obtain authorized source access and attach a comparison locator to every published field before any correction or equipment use is assessed. The pending source discrepancy is left unchanged. Comparison status alone does not authenticate the source evidence. {reference('comparison')}</p>
<h3>Not covered</h3>
<p>The pendulum and motor examples are not worked separately in this report. Motor performance, directional response, torque and drag, buckling, vibration, hydraulics, fatigue, connection ratings, non-magnetic measurement-tool spacing and bending strength ratio criteria are not calculated. Field-name correspondence to drilling data standards is present; an interchange writer is absent. Required criteria remain NOT_EVALUATED. {reference('catalogue')} {reference('implementation')}</p>
</section>

<section id="references"><h2>9. References</h2>
<p>Open references are used for the physical identity and unit conversions. Repository records provide the illustrative inputs, implementation definitions and test inventory.</p>
<h3>9.1 Source use</h3>
<p>No licensed publication is copied or used to set a numerical design limit. The historical nominal-table citation identifies the comparison scope only. The illustrative margin and material density remain assumptions.</p>
<h3>9.2 Sources and locators</h3>
<ol class="refs">
<li id="catalogue">Synthetic illustrative design assumptions, edition v1, checked 16 October 2026. <a href="../../../../data/bha_database/examples.yml">Example catalogue</a>, building assembly and its component records. <a href="../../../bha_database.md">Public module documentation</a>, synthetic slice and usage example. All example inputs are ASSUMED - to be confirmed.</li>
<li id="implementation">Module definitions: <a href="../../../../src/digitalmodel/bha_database/calcs.py">section aggregation and static summary</a>; <a href="../../../../src/digitalmodel/bha_database/checks.py">assembly consistency rules</a>; <a href="../../../../src/digitalmodel/bha_database/schema.py">quantity and geometry schema</a>; <a href="../../../../src/digitalmodel/bha_database/loader.py">bounded loader and manifest checks</a>. These define software behaviour rather than external engineering criteria.</li>
<li id="mechanics">OpenStax, University Physics Volume 1, 2016 edition, section 14.4, “Archimedes’ Principle and Buoyancy”. <a href="https://openstax.org/books/university-physics-volume-1/pages/14-4-archimedes-principle-and-buoyancy">Open textbook</a>, read 22 October 2026. The buoyant-force identity is paraphrased and algebraically reduced for the stated uniform-material model; textbook figures and numerical examples are not copied.</li>
<li id="geometry">OpenStax, University Physics Volume 1, 2016 edition, section 2.1, “Scalars and Vectors”. <a href="https://openstax.org/books/university-physics-volume-1/pages/2-1-scalars-and-vectors">Scalar addition and collinear force addition</a>, read 22 October 2026. Common-unit scalar lengths are added to define section coordinates; weights in the same direction are summed.</li>
<li id="units">NIST Special Publication 811, Guide for the Use of the International System of Units, 2008 edition, web version 3.2, Appendix B.9, Length and Force conversion tables. <a href="https://www.nist.gov/pml/special-publication-811/nist-guide-si-appendix-b-conversion-factors/nist-guide-si-appendix-b9">Conversion tables</a> and <a href="https://www.nist.gov/pml/special-publication-811/nist-guide-si-version-history">edition history</a>, read 22 October 2026. The displayed force factor is rounded to the precision of the public table.</li>
<li id="comparison">ISO 10424-1:2004, Rotary drill stem elements, Table 14: cited scope only; no licensed numerical value is reused. <a href="../../../../src/digitalmodel/bha_database/iso_table14.py">Comparison audit</a>, published-field enumeration and per-cell metadata. <a href="../../../../tests/bha_database/test_table14_audit.py">Comparison tests</a>, metadata counts and pending-state checks. Authorized source comparison remains outstanding.</li>
<li id="tests">Existing module test files: <a href="../../../../tests/bha_database/test_database.py">catalogue tests</a>, <a href="../../../../tests/bha_database/test_iso_table14.py">nominal-table regression tests</a> and <a href="../../../../tests/bha_database/test_table14_audit.py">comparison-coverage tests</a>. Function counts are obtained by parsing these files; parameter cases are not inferred from those counts.</li>
<li id="report-tests"><a href="../../../../tests/bha_database/test_module_report.py">Numerical and rebuild tests</a> and <a href="../../../../scripts/bha_database/build_module_report.py">report generator</a>. Literal anchor arithmetic and display tolerances are stated in the tests. Rebuild from the repository root with <code>PYTHONPATH=src python scripts/bha_database/build_module_report.py</code>.</li>
</ol>
<p>The source-checkout catalogue files are <code>examples.yml</code> and <code>manifest.yaml</code>. The accepted connection labels are <code>NC46</code> and <code>NC50</code>; these labels do not assign thread ratings. The advisory states are <code>WITHIN_ASSUMED_LIMIT</code>, <code>EXCEEDED</code> and <code>NOT_EVALUATED</code>. An absent buoyed weight is returned as <code>None</code>. {reference('implementation')}</p>
<pre><code>from digitalmodel.bha_database import load_database

db = load_database()
summary = db.summary('SYNTHETIC-BUILDING')
sections = db.sections('SYNTHETIC-BUILDING')
# Installed use: load_database(explicit_catalogue_directory)</code></pre>
</section>

<section id="appendix"><h2>10. Appendix</h2>
<p>The example data, detailed returned geometry and quality checks are recorded together. Inputs are kept separate from computed values so the limits of the calculation remain explicit.</p>
<h3>10.1 Data register</h3>
<p>All component inputs in Table A are ASSUMED - to be confirmed. Joint lengths are per joint; every listed count is obtained from the example. The fluid-density basis uses the US liquid gallon. The bit has a nominal bore field for the catalogue model; its cutting geometry is not resolved. {reference('catalogue')}</p>
<table id="input-table"><thead><tr><th>Component</th><th>Count</th><th>Joint length (ft)</th><th>Body diameter (m)</th><th>Bore (m)</th><th>Weight per length (lbf/ft)</th><th>Source</th></tr></thead>
<tbody>{''.join(input_rows)}</tbody><caption>Table A. Synthetic building assembly inputs; units and source basis follow the supplied quantity records.</caption></table>
<table id="data-register"><thead><tr><th>Input or missing criterion</th><th>Value or disposition</th><th>Basis and action</th><th>Source</th></tr></thead><tbody>
<tr><td>Fluid density (lb/US gal)</td><td>{n('mud_density')}</td><td>ASSUMED - to be confirmed; replace for the actual fluid.</td><td><a href="#catalogue">Assumed catalogue</a></td></tr>
<tr><td>Material density (lb/US gal)</td><td>{n('material_density')}</td><td>ASSUMED - to be confirmed; uniform material approximation.</td><td><a href="#catalogue">Assumed catalogue</a></td></tr>
<tr><td>Advisory margin (%)</td><td>{n('margin')}</td><td>ASSUMED - to be confirmed; no prescribed design-code value.</td><td><a href="#catalogue">Assumed catalogue</a></td></tr>
<tr><td>Planned load (lbf)</td><td>{n('planned_wob_lbf')}</td><td>ASSUMED - to be confirmed; illustrative demand.</td><td><a href="#catalogue">Assumed catalogue</a></td></tr>
<tr><td>Hole diameter (in)</td><td>{n('hole_in')}</td><td>ASSUMED - to be confirmed; equality checks nominal geometry only.</td><td><a href="#catalogue">Assumed catalogue</a></td></tr>
<tr><td>Blade length and bottom offset (m)</td><td>{n('blade_length_m')}; {n('blade_offset_m')}</td><td>ASSUMED - to be confirmed; the first joint supplies the reported blade centre.</td><td><a href="#catalogue">Assumed catalogue</a></td></tr>
<tr><td>Operating clearance, strength and spacing criteria</td><td>NOT_EVALUATED</td><td>Required inputs; no substitute numerical limit is assigned.</td><td><a href="#implementation">Scope of checks</a></td></tr>
</tbody><caption>Table B. Input basis and required engineering criteria.</caption></table>
<h3>10.2 Detailed outputs</h3>
<p>Returned section lengths include joint counts and overrides. Distance is measured to the lower end of each section from the bit datum; the diameter envelope includes any blade diameter. {reference('implementation')}</p>
<table id="output-table"><thead><tr><th>Component</th><th>Section length (m)</th><th>Distance from bit (m)</th><th>Maximum diameter (m)</th><th>Source</th></tr></thead>
<tbody>{''.join(output_rows)}</tbody><caption>Table C. Module-returned geometry and the component diameter envelope used in Figure B.</caption></table>
<h3>10.3 Quality pre-flight evidence</h3>
<p>The evidence set is limited to the documented example and the cited module files. The checks below state their scope; they do not substitute for field validation.</p>
<table><thead><tr><th>Evidence</th><th>Scope established</th><th>Source</th></tr></thead><tbody>
<tr><td>Documented callable interface</td><td>The database loader and assembly summary are exercised by the example and report tests.</td><td><a href="#report-tests">Numerical checks</a></td></tr>
<tr><td>Hand-calculation anchors</td><td>Length, diameter, blade centre and static weights are compared with literal expected values.</td><td><a href="#report-tests">Anchor comments and assertions</a></td></tr>
<tr><td>Presentation consistency</td><td>Annotated numbers are checked at their declared display precision; rebuilding compares the delivered page.</td><td><a href="#report-tests">Display and rebuild checks</a></td></tr>
<tr><td>Engineering acceptance</td><td>NOT_EVALUATED; measured performance and qualified criteria are absent from this evidence set.</td><td><a href="#catalogue">Synthetic basis</a></td></tr>
</tbody><caption>Table D. Evidence available for this report and its limits.</caption></table>
<table><thead><tr><th>Evidence group</th><th>Test functions</th><th>Source</th></tr></thead>
<tbody>{inventory_rows}</tbody><caption>Table E. Existing module source inventory; parameter expansion and execution are separate from function counts.</caption></table>
</section>

<aside aria-label="Report review tools">
<details id="change-panel"><summary>Engineering changes</summary>
<label for="previous-revision">Last revision read</label>
<select id="previous-revision"><option value="0">No previous revision</option><option value="1">Revision 1</option></select>
<ul id="change-list"></ul></details>
<details id="comments"><summary>Section comments</summary>
<p>Select a section and enter a review comment. Save the comment file beside this report. Reopen that file to continue a review. Comments remain in this page until saved; closing it discards unsaved comments.</p>
<label for="review-section">Report section</label><select id="review-section"></select>
<label for="review-comment">Comment</label><textarea id="review-comment"></textarea>
<button id="add-comment" type="button">Add comment</button>
<button id="save-comments" type="button">Save comments beside report</button>
<label for="import-comments">Open saved comments</label><input id="import-comments" type="file" accept="application/json">
<p id="comment-status" aria-live="polite">No saved comment file is open.</p><ol id="comment-list"></ol>
</details></aside>
<footer>Bottom hole assembly database analysis · Draft for maintainer review · Synthetic teaching basis.</footer>
</main>
<script id="change-data" type="application/json">{changes}</script>
<script>
"use strict";
const byId = id => document.getElementById(id);
document.querySelectorAll('.datum').forEach(datum => {{
  const detail = () => {{ byId('diameter-detail').textContent = datum.querySelector('title').textContent; }};
  datum.addEventListener('pointerenter', detail);
  datum.addEventListener('focus', detail);
}});
const changes = JSON.parse(byId('change-data').textContent);
function showChanges() {{
  byId('change-list').replaceChildren();
  const previous = Number(byId('previous-revision').value);
  changes.revisions.filter(item => Number(item.revision) > previous).forEach(item => {{
    const li = document.createElement('li'); li.textContent = item.description;
    byId('change-list').append(li);
  }});
  if (!byId('change-list').children.length) {{
    const li = document.createElement('li'); li.textContent = 'No engineering changes since the selected revision.';
    byId('change-list').append(li);
  }}
}}
byId('previous-revision').addEventListener('change', showChanges); showChanges();
const sectionIds = new Set();
document.querySelectorAll('main > section').forEach(section => {{
  const option = document.createElement('option'); option.value = section.id;
  option.textContent = section.querySelector('h2').textContent;
  byId('review-section').append(option); sectionIds.add(section.id);
}});
let comments = [];
function displayComments() {{
  byId('comment-list').replaceChildren();
  comments.forEach(comment => {{
    const li = document.createElement('li');
    li.textContent = document.getElementById(comment.section).querySelector('h2').textContent + ': ' + comment.text;
    byId('comment-list').append(li);
  }});
}}
byId('add-comment').addEventListener('click', () => {{
  const text = byId('review-comment').value.trim();
  if (!text) return;
  comments.push({{section: byId('review-section').value, text}});
  byId('review-comment').value = ''; displayComments();
  byId('comment-status').textContent = 'Comments changed; save the file beside the report.';
}});
byId('import-comments').addEventListener('change', async event => {{
  const file = event.target.files[0]; if (!file) return;
  try {{
    const saved = JSON.parse(await file.text());
    if (saved.report !== 'bottom-hole-assembly-database' || saved.revision !== '1' || !Array.isArray(saved.comments) ||
        !saved.comments.every(c => c && sectionIds.has(c.section) && typeof c.text === 'string')) {{
      throw new Error('The comment file does not match this report revision.');
    }}
    comments = saved.comments; displayComments();
    byId('comment-status').textContent = 'Saved comments opened; save again after editing.';
  }} catch (error) {{ byId('comment-status').textContent = 'The comment file was not opened: ' + error.message; }}
}});
byId('save-comments').addEventListener('click', async () => {{
  const body = JSON.stringify({{report:'bottom-hole-assembly-database', revision:'1', comments}}, null, 2) + '\\n';
  try {{
    if (typeof window.showSaveFilePicker === 'function') {{
      const handle = await window.showSaveFilePicker({{suggestedName:'report.comments.json',
        types:[{{description:'Comment file', accept:{{'application/json':['.json']}}}}]}});
      const writable = await handle.createWritable(); await writable.write(body); await writable.close();
      byId('comment-status').textContent = 'Comments saved to the selected file.';
    }} else {{
      const url = URL.createObjectURL(new Blob([body], {{type:'application/json'}}));
      const link = document.createElement('a'); link.href = url; link.download = 'report.comments.json';
      document.body.append(link); link.click(); link.remove(); setTimeout(() => URL.revokeObjectURL(url), 1000);
      byId('comment-status').textContent = 'Comment download requested; place report.comments.json beside the report.';
    }}
  }} catch (error) {{
    byId('comment-status').textContent = error.name === 'AbortError' ? 'Save cancelled; comments remain unsaved.' : 'Comments were not saved: ' + error.message;
  }}
}});
</script>
</body></html>
'''


def main():
    """Build the report at its delivery location or an explicit output path."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, default=OUTPUT)
    args = parser.parse_args()
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(render(collect()), encoding="utf-8", newline="\n")


if __name__ == "__main__":
    main()
