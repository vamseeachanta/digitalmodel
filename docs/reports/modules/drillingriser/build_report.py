# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Rebuild the self-contained drilling riser module analysis from public inputs.

Run with PYTHONPATH=src and pass --scratch to a writable temporary directory.
Only the report is a deliverable; the workflow CSV is written under --scratch.
"""

from __future__ import annotations

import argparse
import ast
import copy
import csv
import html
import json
import re
from pathlib import Path

import yaml

from digitalmodel.drilling_riser import adapter, stackup
from digitalmodel.drilling_riser.workflow import router

ROOT = Path(__file__).resolve().parents[4]
HERE = Path(__file__).resolve().parent
EXAMPLE = ROOT / "examples/workflows/riser-stackup/input.yml"
EXEMPLAR = ROOT / "docs/api/cfd/turbulent-flat-plate-verification.html"
BRAND = ROOT / "docs/api/_assets/brand.css"

# Display conversions use the module's component-adapter convention, including
# its finite-precision force and pressure constants. No new material data enter.
CONVERSIONS = {
    "kip": 1 / adapter.KIPS_TO_KN,
    "in": 1 / adapter._IN_TO_MM,
    "ft": 1 / adapter._FT_TO_M,
    "psi": 1 / adapter._PSI_TO_MPA,
    "ft²": 1 / adapter._FT_TO_M**2,
    "kip/ft³": adapter._FT_TO_M**3 / adapter.KIPS_TO_KN,
    "%": 100.0,
}


def compute(scratch: Path, cfg: dict | None = None) -> dict:
    """Call the documented router; keep scratch paths out of returned data."""
    cfg = copy.deepcopy(cfg) if cfg is not None else yaml.safe_load(EXAMPLE.read_text())
    inputs = dict(cfg["riser_stackup"])
    cfg["_config_dir_path"] = str(scratch.resolve())
    cfg["_config_file_path"] = str(scratch.resolve() / "input.yml")
    # A supplied configuration cannot redirect calculation residue elsewhere.
    cfg["riser_stackup"]["output_dir"] = str(scratch.resolve() / "results")
    summary = router(cfg)["riser_stackup"]
    with Path(summary.pop("profile_csv")).open(newline="") as stream:
        profile = [{k: float(v) for k, v in row.items()} for row in csv.DictReader(stream)]
    # Independent force balance, separate from effective_tension().
    top = inputs["submerged_weight_kn"] * inputs["dynamic_factor"]
    hand = [top - inputs["submerged_weight_kn"] * row["depth_factor"] for row in profile]
    return {
        "inputs": inputs,
        "summary": summary,
        "profile": profile,
        "hand_profile_kn": hand,
        "minimum_effective_kn": min(row["effective_tension_kn"] for row in profile),
        "tension_difference_pct": max(abs(row["effective_tension_kn"] - ref) / abs(ref) * 100
                                      for row, ref in zip(profile, hand)),
        "defaults": {"weight_factor": stackup.F_WT_DEFAULT, "buoyancy_factor": stackup.F_BT_DEFAULT},
        "changes": json.loads((HERE / "changes.json").read_text()),
    }


def metric_value(data: dict, key: str, unit: str) -> float:
    value = data
    for part in key.split("."):
        value = value[int(part)] if isinstance(value, list) else value[part]
    scale = 1 if key.endswith("_pct") else CONVERSIONS.get(unit, 1)
    return float(value) * scale


def format_value(value: float, unit: str) -> str:
    if value == 0:
        value = 0.0
    precision = 1
    if unit in ("—", "kN/m³"):
        precision = 2
    if unit in ("in", "kip/ft³", "ft²", "m²"):
        precision = 3
    if value and round(value, precision) == 0:
        return f"{value:.1e}".replace("-", "−")
    return f"{value:,.{precision}f}".replace("-", "−")


def number(data: dict, key: str, unit: str = "—") -> str:
    value = metric_value(data, key, unit)
    return (f'<span data-metric="{key}" data-unit="{unit}" data-value="{value!r}">'
            f'{format_value(value, unit)}</span>')


def chart_data(data: dict) -> dict:
    """Retain full precision internally; labels use engineering precision."""
    return {
        "depth_pct": [r["depth_factor"] * CONVERSIONS["%"] for r in data["profile"]],
        "module_kip": [r["effective_tension_kn"] * CONVERSIONS["kip"] for r in data["profile"]],
        "hand_kip": [v * CONVERSIONS["kip"] for v in data["hand_profile_kn"]],
        "limit_kip": 0.0,
        "case": "Public stack-up example",
        "component": "Uniform equivalent string",
    }


def styles() -> str:
    """Inline existing house CSS unchanged except non-rendered comments."""
    exemplar = re.search(r"<style>(.*?)</style>", EXEMPLAR.read_text(), re.S).group(1)
    css = re.sub(r"/\*.*?\*/", "", BRAND.read_text() + "\n" + exemplar, flags=re.S)
    return "\n".join(line.rstrip() for line in css.splitlines())


def input_table(data: dict, native: bool = False) -> str:
    fields = [
        ("Submerged weight", "submerged_weight_kn", "kip", "kN"),
        ("Weight multiplier", "dynamic_factor", "—", "—"),
        ("Outside diameter", "od_mm", "in", "mm"),
        ("Design internal pressure", "design_pressure_mpa", "psi", "MPa"),
        ("Specified minimum yield strength", "smys_mpa", "psi", "MPa"),
        ("Wall multiplier", "barlow_safety_factor", "—", "—"),
        ("Buoyancy uplift", "buoyancy_uplift_kn", "kip", "kN"),
        ("Internal area", "internal_area_m2", "ft²", "m²"),
        ("Mud weight density", "mud_density_kn_m3", "kip/ft³", "kN/m³"),
        ("Mud column head", "mud_column_m", "ft", "m"),
        ("Seawater weight density", "seawater_density_kn_m3", "kip/ft³", "kN/m³"),
        ("Seawater column head", "seawater_column_m", "ft", "m"),
    ]
    rows = "".join(f'<tr><td>{label}</td><td>{number(data, "inputs." + key, si if native else us)}</td>'
                   f'<td>{si if native else us}</td><td><a href="#ref-example">Example input</a></td></tr>'
                   for label, key, us, si in fields)
    return ('<table><thead><tr><th>Quantity</th><th>Value</th><th>Unit</th><th>Source</th></tr></thead>'
            f'<tbody>{rows}</tbody><caption>Public example inputs; '
            f'{"native module units" if native else "engineering display units"}.</caption></table>')


def inventory() -> str:
    files = [
        ("tests/drilling_riser/test_stackup_doc_verified.py", "Scalar arithmetic and trend assertions"),
        ("tests/drilling_riser/test_riser_response.py", "Pinned-beam closed forms, shear and trends"),
        ("tests/drilling_riser/test_drive_off_worked_example.py", "Input-gap evidence; reproduction is explicitly skipped"),
        ("tests/drilling_riser/test_module_report.py", "Report rebuild, public-module reconciliation and numerical bindings"),
    ]
    rows = []
    for path, scope in files:
        tree = ast.parse((ROOT / path).read_text())
        count = sum(isinstance(n, ast.FunctionDef) and n.name.startswith("test_") for n in ast.walk(tree))
        rows.append(f'<tr><td>{scope}</td><td>{count}</td><td><code>{path}</code></td></tr>')
    return ('<table><thead><tr><th>Evidence held</th><th>Test definitions</th><th>Source</th></tr></thead><tbody>'
            + ''.join(rows) + '</tbody><caption>Definition counts are calculated from source; '
            'parameterised test cases and execution results are separate quantities.</caption></table>')


SCHEMATIC = """
<figure><svg viewBox="0 0 760 390" role="img" aria-labelledby="stack-title stack-description">
<title id="stack-title">Drilling riser stack-up and calculation idealisation</title>
<desc id="stack-description">A host supports a tensioned riser above a seabed wellhead. The physical stack
is shown at left and the uniform-weight axial idealisation at right. Component dimensions and supports are missing.</desc>
<g fill="none" stroke="currentColor" stroke-width="2">
<path d="M40 65 L250 65 L225 100 L70 100 Z M90 65 L90 35 L180 35 L180 65"/>
<path d="M15 112 Q40 104 65 112 T115 112 T165 112 T215 112 T265 112"/>
<path d="M145 100 L145 150 M135 150 L155 150 L155 185 L135 185 Z M142 185 L142 320 M148 185 L148 320"/>
<rect x="126" y="320" width="38" height="30"/>
<path d="M20 355 L270 355 M145 350 L145 375"/>
<path d="M460 110 L460 335 M450 110 L460 85 L470 110 M450 160 L460 180 L470 160 M450 220 L460 240 L470 220 M450 280 L460 300 L470 280"/>
<path d="M450 335 L470 335 M450 335 L460 350 L470 335"/>
</g>
<g fill="currentColor" font-size="13">
<text x="275" y="57">Host motion: not supplied</text>
<text x="175" y="168">Telescopic joint</text><text x="175" y="187">travel not supplied</text>
<text x="25" y="235">Riser</text><text x="25" y="253">joints and foam</text><text x="25" y="271">zones unknown</text>
<text x="175" y="330">Blowout</text><text x="175" y="347">preventer</text>
<text x="27" y="377">Seabed and wellhead</text>
<text x="490" y="93">Applied top force T</text><text x="490" y="126">Surface: depth fraction d = 0</text>
<text x="490" y="185">Uniform effective weight W</text><text x="490" y="226">Axial force balance only</text>
<text x="490" y="267">No lateral or rotational restraint</text><text x="490" y="286">is represented by this calculation</text>
<text x="490" y="342">Lower end: depth fraction d = 1</text>
<text x="490" y="369">Effective force T − W</text>
</g></svg><figcaption>System schematic and axial model, not to scale. The physical components are indicative;
no discrete component geometry is inferred from the aggregate-weight example.</figcaption></figure>
"""


CHART = """
<figure><svg id="tension-chart" viewBox="0 0 760 390" role="img" aria-labelledby="chart-title chart-description">
<title id="chart-title">Still-water effective tension</title>
<desc id="chart-description">Hand equilibrium is a line and module values are markers. The zero-effective-tension
reference is drawn. Focus a marker or move the pointer over it to read the demand and reference.</desc>
</svg><p id="chart-readout" role="status">Focus a marker to read the tension and its zero reference.</p>
<figcaption>Effective tension against depth fraction. The zero reference denotes loss of tensile effective force,
not a complete acceptance criterion. Buoyancy-zone boundaries are unavailable. Wall tension is not evaluated.</figcaption></figure>
"""


INTERACTION = r"""
<script>
'use strict';
const data = JSON.parse(document.getElementById('engineering-data').textContent);
const svg = document.getElementById('tension-chart');
const ns = 'http://www.w3.org/2000/svg';
function node(tag, attrs, text) {
  const element = document.createElementNS(ns, tag);
  Object.entries(attrs).forEach(([key, value]) => element.setAttribute(key, value));
  if (text !== undefined) element.textContent = text;
  svg.appendChild(element); return element;
}
const low = Math.min(data.limit_kip, ...data.module_kip), high = Math.max(...data.module_kip);
const span = high - low || 1;
const x = value => 95 + (value - low) / span * 585;
const y = depth => 55 + depth / 100 * 260;
node('path', {d:'M95 45 V325 H700', fill:'none', stroke:'currentColor'});
node('line', {x1:x(data.limit_kip), x2:x(data.limit_kip), y1:45, y2:325,
             stroke:'var(--nogo)', 'stroke-dasharray':'5 4'});
node('text', {x:100, y:25, fill:'currentColor'}, 'Hand equilibrium — line; module — markers; zero reference — dashed');
node('text', {x:260, y:377, fill:'currentColor'}, 'Effective tension (kip)');
node('text', {x:15, y:205, fill:'currentColor', transform:'rotate(-90 15 205)'}, 'Depth fraction (%)');
node('text', {x:x(data.limit_kip) + 5, y:340, fill:'currentColor'}, '0.0 kip');
node('text', {x:x(high) - 40, y:340, fill:'currentColor'}, high.toFixed(1));
node('polyline', {points:data.hand_kip.map((t,i)=>`${x(t)},${y(data.depth_pct[i])}`).join(' '),
                  fill:'none', stroke:'var(--brand)', 'stroke-width':2});
data.module_kip.forEach((t, i) => {
  const depth = data.depth_pct[i];
  node('text', {x:45, y:y(depth)+4, fill:'currentColor'}, depth.toFixed(1));
  const label = `${data.case}; ${data.component}; depth fraction ${depth.toFixed(1)}%; demand ${t.toFixed(1)} kip; zero reference ${data.limit_kip.toFixed(1)} kip; equipment limit not established`;
  const dot = node('circle', {cx:x(t), cy:y(depth), r:6, fill:'var(--brand)', tabindex:0,
                            role:'img', 'aria-label':label});
  const title = document.createElementNS(ns,'title'); title.textContent = label; dot.appendChild(title);
  ['focus','mouseenter'].forEach(event => dot.addEventListener(event, () => {
    document.getElementById('chart-readout').textContent = label;
  }));
});
const changes = JSON.parse(document.getElementById('change-data').textContent);
const changeList = document.getElementById('change-list');
changes.changes.forEach(change => {
  const p = document.createElement('p'), a = document.createElement('a');
  a.href = '#' + change.section; a.textContent = change.description;
  p.appendChild(a); changeList.appendChild(p);
});
let comments = [], selectedSection = 'summary', selectedText = '', directory;
const status = document.getElementById('comment-status');
const commentList = document.getElementById('comment-list');
function showComments() {
  commentList.replaceChildren();
  comments.forEach(comment => {
    const p = document.createElement('p'), context = document.createElement('span');
    context.textContent = comment.section + (comment.quote ? ': ' + comment.quote : '') + ' — ';
    const text = document.createElement('span'); text.textContent = comment.text;
    p.append(context,text); commentList.appendChild(p);
  });
}
function parseComments(text) {
  const parsed = JSON.parse(text);
  if (parsed.revision !== changes.revision || !Array.isArray(parsed.comments) || parsed.comments.length > 1000 ||
      !parsed.comments.every(c => c && typeof c.text === 'string' && c.text.length <= 5000 &&
         typeof c.section === 'string' && document.getElementById(c.section)?.tagName === 'SECTION' &&
         typeof c.quote === 'string' && c.quote.length <= 1000)) throw new Error('Invalid comments');
  return parsed.comments;
}
document.querySelectorAll('main > section').forEach(section => {
  section.addEventListener('mouseup', () => {
    const selection = window.getSelection().toString().trim();
    if (selection) { selectedSection = section.id; selectedText = selection.slice(0,1000);
      document.getElementById('comment-context').textContent = selectedText; }
  });
});
document.getElementById('comment-section').addEventListener('change', event => {
  selectedSection = event.target.value; selectedText = '';
  document.getElementById('comment-context').textContent = selectedSection;
});
document.getElementById('add-comment').addEventListener('click', () => {
  const input = document.getElementById('comment-text'), text = input.value.trim();
  if (!text) { status.textContent = 'Enter a comment before adding it.'; return; }
  comments.push({section:selectedSection, quote:selectedText, text}); input.value = '';
  showComments(); status.textContent = 'Comment added in memory. Save the comments before closing.';
});
document.getElementById('save-comments').addEventListener('click', async () => {
  try {
    if (!window.showDirectoryPicker) {
      status.textContent = 'Folder saving is unavailable. Use Download comments and save the file beside this report.';
      return;
    }
    directory = directory || await window.showDirectoryPicker({mode:'readwrite'});
    const file = await directory.getFileHandle('report.comments.json', {create:true});
    const contents = await (await file.getFile()).text();
    const held = contents.trim() ? parseComments(contents) : [];
    const merged = new Map();
    [...held,...comments].forEach(c => merged.set(JSON.stringify([c.section,c.quote,c.text]),c));
    const combined = [...merged.values()];
    const writable = await file.createWritable();
    await writable.write(JSON.stringify({revision:changes.revision, comments:combined}, null, 2) + '\n');
    await writable.close(); comments=combined; showComments();
    status.textContent = 'Comments saved in the selected folder; existing comments are retained.';
  } catch { status.textContent = 'Comments were not saved. Check the folder and existing comment file, or use Download comments.'; }
});
document.getElementById('download-comments').addEventListener('click', () => {
  const url = URL.createObjectURL(new Blob([JSON.stringify({revision:changes.revision, comments},null,2)+'\n'],{type:'application/json'}));
  const a = document.createElement('a'); a.href=url; a.download='report.comments.json'; a.click();
  setTimeout(()=>URL.revokeObjectURL(url),1000);
  status.textContent = 'Save report.comments.json beside this report; the download location is selected by the browser.';
});
document.getElementById('load-comments').addEventListener('change', async event => {
  try {
    const file = event.target.files[0]; if (!file) return;
    comments = parseComments(await file.text()); showComments(); status.textContent = 'Saved comments loaded.';
  } catch { status.textContent = 'Comments were not loaded: the file or revision is invalid.'; }
});
</script>
"""


def render(data: dict) -> str:
    """Write the engineering narrative in one template with bound numbers."""
    n = lambda key, unit="—": number(data, key, unit)
    revision = html.escape(data["changes"]["revision"])
    date = html.escape(data["changes"]["date"])
    profile_rows = ''.join(
        f'<tr><td>{n(f"profile.{i}.depth_factor", "%")}</td>'
        f'<td>{n(f"profile.{i}.effective_tension_kn", "kip")}</td>'
        '<td><a href="#ref-code">Axial force balance</a></td></tr>'
        for i in range(len(data["profile"]))
    )
    body = f"""
<header><p class="kicker">Drilling riser module analysis</p>
<h1>Drilling riser stack-up analysis</h1><p>Draft for client review</p>
<p class="sub">Public example calculation · Revision {revision} · {date}</p></header>
<main>
<section id="summary"><h2 id="summary-heading">1. Executive Summary</h2>
<p>The drilling riser is a tensioned conduit between a floating host and a seabed wellhead.
Analysis is performed to determine the capabilities and limits of the drilling riser module;
the key results are summarized below.</p>
<ul><li>The public stack-up example is reproduced as an axial screening calculation.
<ul><li>The applied top-force estimate is {n('summary.top_tension_required_kn', 'kip')} kip
and the lowest effective force is {n('minimum_effective_kn', 'kip')} kip against the zero-effective-tension reference.</li>
<li>The pressure-only wall estimate is {n('summary.wall_thickness_required_mm', 'in')} in;
actual wall thickness and allowances are not supplied, so a wall acceptance result is not established.</li></ul></li>
<li>No operating window is established. The allowable drilling and connected offsets are not established.
<ul><li>A watch circle and equipment acceptance criteria are absent; do not connect on the basis of this calculation.</li>
<li>Dynamic response, recoil and fatigue are not evaluated.</li></ul></li></ul>
<h3>Revision history</h3><table><thead><tr><th>Revision</th><th>Date</th><th>Change and reason</th></tr></thead>
<tbody><tr><td>{revision}</td><td>{date}</td><td>The public calculation and its limits are documented to distinguish arithmetic evidence from operating acceptance.</td></tr></tbody></table>
{CHART}
<details><summary>Engineering changes</summary><div id="change-list"></div></details>
</section>
<section id="intro"><h2 id="intro-heading">2. Introduction</h2>
<p>The drilling riser calculation package is used for component assembly, axial sizing, static response and operating assessments.
Riser analysis is performed here for the registered public stack-up example; no site water depth is identified.</p>
<p>The main workflow accepts aggregate weight, pressure and geometry, calculates force and pressure-only thickness,
and writes an effective-tension profile. The documented router is called directly, without a licensed solver.
The example is public repository documentation, not an independently measured field benchmark.</p>
</section>
<section id="data"><h2 id="data-heading">3. Design Data</h2>
<p>The published example inputs are retained without substitutions. Missing physical and equipment data are identified in the appendix.</p>
<h3>3.1 Field, host and stack-up</h3>
<p>No site, water depth, host geometry, response amplitude operators, offset criterion or component schedule is supplied.
The equal mud and seawater column heads are input pressure heads; neither is relabelled as water depth.
The stack-up is therefore drawn schematically, with its gaps identified.</p>
{SCHEMATIC}
<table><thead><tr><th>Component or support</th><th>Data held</th><th>Gap and effect</th></tr></thead><tbody>
<tr><td>Host and tensioner</td><td>Calculated top-force screen</td><td>Capacity, failures, efficiency and support geometry absent; no equipment acceptance.</td></tr>
<tr><td>Telescopic joint</td><td>Indicative location only</td><td>Configuration, travel and stops absent; no stroke assessment.</td></tr>
<tr><td>Main riser and buoyancy</td><td>Aggregate weight and uplift</td><td>Joint lengths, foam boundaries, adjacent diameters and stiffness absent; no component tension distribution.</td></tr>
<tr><td>Lower flex joint and wellhead</td><td>Indicative location only</td><td>Rotational stiffness, conductor support and allowable reactions absent.</td></tr>
</tbody><caption>Physical stack-up gaps read with the schematic.</caption></table>
<h3>3.2 Contents and pressure</h3>
<p>Input values are shown in engineering display units. The native-unit register is retained in the appendix.
Weight density is shown directly; a mass-density convention for pounds per gallon is not inferred.</p>
{input_table(data)}
<h3>3.3 Metocean, criteria and load cases</h3>
<p>The illustrated profile is still-water axial equilibrium. Current-dominated and wave-dominated environmental sets are absent.
No return-period, sea-state, heading or offset matrix is supplied.</p>
<p>The zero-effective-tension line is a force-sign reference from the implemented axial equilibrium, not a drilling acceptance limit.
The weight and wall multipliers are example assumptions, not verified standard acceptance criteria.
The default slip-ring weight and buoyancy factors are inherited implementation constants; their recorded source and qualification boundary are given in References.</p>
</section>
<section id="method"><h2 id="method-heading">4. Analysis Methodology</h2>
<p>The axial calculation is evaluated first by hand equilibrium and then by the module.
The boundary-condition idealisation is shown beside the physical stack-up; no finite-element mesh, centralisers, lateral supports or stops are represented.</p>
<h3>4.1 Equations and symbols</h3>
<p>The applied top force is T = W F, and the effective force at depth fraction d is E(d) = T − W d.
W is the example submerged string weight, F is the example weight multiplier, T is the calculated top force,
d is the fraction measured downward from the surface, and E is effective tension. The profile endpoints are surface and lower end.
These equations are implemented screening algebra, sourced to the calculation functions in References.</p>
<p>The pressure-only wall estimate is t = P D S / (2 Y).
t denotes required wall thickness, P is design internal pressure, D is outside diameter,
S is the example wall multiplier, and Y is specified minimum yield strength.
The coefficient follows the implemented Barlow thin-wall hoop-equilibrium relation; external pressure, bending,
corrosion, wear and manufacturing allowances are not included.</p>
<p>The slip-ring expression is R = W f − B b + A(γm Hm − γw Hw).
R is minimum slip-ring force, B is buoyancy uplift, f and b are weight and buoyancy factors,
A is the internal area, γm and γw are mud and seawater weight densities, and Hm and Hw are their vertical heads.
The implementation expects flooded-steel apparent weight with fluid head counted once.
The example does not document that weight convention, so its slip-ring result remains illustrative.</p>
<h3>4.2 Calculation sequence and outputs</h3>
<p>The public input is read, the documented router is called, and its profile CSV is read back.
The returned scalars are top force, pressure-only wall thickness, optional slip-ring force and profile point count.
The CSV contains depth fraction and effective force. The module uses kilonewtons, millimetres and megapascals internally;
engineering displays use the component adapter's conversion convention.</p>
<p>The build-up beyond still water is not performed. Static current response needs distributed drag and section stiffness;
wave-current dynamics need response amplitude operators, sea-state spectra, headings, initial conditions and travel limits.
Regular heave alone is not substituted for host-motion response.</p>
</section>
<section id="results"><h2 id="results-heading">5. Detailed Results</h2>
<p>The scalar results and axial profile are calculated from the public example.
The effective-force chart precedes environmental and dynamic assessments because it is the available static arithmetic check.</p>
<h3>5.1 Statics against hand equilibrium</h3>
<p>The module returns {n('summary.top_tension_required_kn', 'kip')} kip top force and
{n('minimum_effective_kn', 'kip')} kip lowest effective force against the zero-effective-tension reference.
The maximum absolute relative difference from hand equilibrium is {n('tension_difference_pct', '%')} %,
using hand equilibrium as the reference at the supplied profile stations.
This checks arithmetic consistency of the assumed linear profile; it is not independent qualification of the physical model.</p>
<p>The wall estimate is {n('summary.wall_thickness_required_mm', 'in')} in.
The illustrative slip-ring force is {n('summary.min_slip_ring_tension_kn', 'kip')} kip.
The router reports these separately and does not reconcile them into a governing tensioner setting.
The flat weight multiplier omits surviving tensioner count, efficiency and angular allowances;
its result is not the package's separate minimum-tensioner-capacity calculation.</p>
<p>Wall tension is not evaluated. Effective tension and true wall force are different quantities;
a wall-force curve is not fabricated from the aggregate weight profile. Foam-zone locations are also absent.</p>
<h3>5.2 Current and operating envelopes</h3>
<p>Static current response is not evaluated. Current-dominated environments and wave-dominated environments
have no supplied case sets or acceptance criteria, so no offset envelope is established for drilling or staying connected.
The static tension chart is not an operating-envelope chart.</p>
<h3>5.3 Dynamics, drift-off, recoil and weak point</h3>
<p>Dynamic stroke, tension, angles and stop strikes are not evaluated. Host-motion response data and equipment travel limits are absent.</p>
<p>Drift-off under the annual current is not evaluated; drift-off under the decadal current is not evaluated.
No disconnect time or watch circle is established; do not connect on this basis.</p>
<p>Recoil is not evaluated: data needed: closure curves. Weak point analysis is not performed.
Vortex-induced vibration and fatigue are not evaluated.</p>
</section>
<section id="findings"><h2 id="findings-heading">6. Key Findings</h2>
<p>Public calculation entry points and test evidence are present. Their existence supports reproducible arithmetic,
while operating acceptance remains outside the supplied example data.</p>
<p>The effective-force profile stays above the zero-effective-tension reference in this example.
That force-sign observation does not establish tensioner capacity, structural strength, travel margin or safe connection.</p>
<p>The separate pressure-only wall and slip-ring estimates are returned without a combined design decision.
The mass, buoyancy and fluid-head conventions require reconciliation before the slip-ring value is used as a setting.</p>
</section>
<section id="limits"><h2 id="limits-heading">7. Limitations</h2>
<p>The example is an aggregate axial screen. Its output is limited by the supplied weight convention,
unverified acceptance factors and absent component and environmental data.</p>
<p>The scalar helpers perform arithmetic without comprehensive finite-value, positivity or geometry validation.
The router checks depth-grid ordering and range and rejects incomplete optional slip-ring fields,
but scalar plausibility remains a caller responsibility. No standard compliance or equipment certification is asserted.</p>
<h3>7.1 Not covered</h3>
<p>Component-registry assembly, schedule reconciliation, taper stress-joint sizing, static beam and conductor response,
operating envelopes, telemetry monitoring, drift-off screening, response correction, damping, tool passage,
stack-up drawing reconciliation, global-model preparation and result aggregation are separate capabilities.
They are identified from the module source but are not exercised by the worked stack-up example.</p>
<p>The paper-based drive-off fixture is explicitly recorded as incomplete and incompatible with the scalar drift-off screen.
Its reproduction test is skipped; it is not a successful published benchmark. The repository holds closed-form static-beam tests,
but those tests do not qualify the operating envelope or coupled dynamic response.</p>
</section>
<section id="recommendations"><h2 id="recommendations-heading">8. Recommendations</h2>
<p>A complete operating assessment is recommended before a connection decision is made.
The required work follows the missing inputs and model boundaries documented above.</p>
<p>Reconcile flooded-steel weight, buoyancy and fluid-head accounting against the actual component schedule,
then apply the separate surviving-tensioner calculation with documented capacity, efficiency and angular allowances.
The source for this separation is the assembly and slip-ring function documentation in References.</p>
<p>Establish the actual wall, corrosion and wear allowances, pressure differential and combined-load criteria before comparing thickness.
Populate component stiffness, buoyancy zones, support locations, host response amplitude operators,
metocean sets, joint travel, disconnect duration and closure curves before calculating operating limits.
Retain the zero-force screen as a diagnostic and require a complete watch-circle assessment before connection.</p>
<p>Extend automated input checks to reject non-finite values and physically inconsistent geometry.
Retain the independent equilibrium comparison and add publicly sourced component and dynamic benchmarks with complete inputs,
compatible model assumptions and declared tolerances before extending the stated validation scope.</p>
</section>
<section id="references"><h2 id="references-heading">9. References</h2>
<p>Repository records are used as the primary calculation evidence. External publication titles below identify
implementation provenance only; publication clauses have not been independently checked for this analysis.</p>
<h3>9.1 Basis and source status</h3>
<p id="ref-example">The registered public stack-up example supplies every case input and the explicit weight and wall multipliers.
These are example assumptions, not measured design data. The workflow registry identifies the example as an offline calculation.</p>
<p id="ref-code">The stack-up calculation functions and router define all equations and output conventions used here.
The component adapter defines the display conversions. The default weight factor {n('defaults.weight_factor')}
and buoyancy factor {n('defaults.buoyancy_factor')} are read from the implementation, without substituting a value from a publication.</p>
<p>The implementation cites American Bureau of Shipping, Guidance Notes on Drilling Riser Analysis, 2017,
Section 3, Subsection 3, item iii), equation (3.2), and American Petroleum Institute Recommended Practice 16Q,
Design, Selection, Operation and Maintenance of Marine Drilling Riser Systems, second edition, 2017.
The latter clause text is recorded as pending a held-edition check. No licensed table or clause values are reproduced as independent acceptance criteria.</p>
<h3>9.2 Repository locators and evidence inventory</h3>
<ul><li><code>examples/workflows/riser-stackup/input.yml</code>: complete public case inputs.</li>
<li><code>docs/registry/workflows.yaml</code>: registered stack-up workflow and expected CSV output.</li>
<li><code>src/digitalmodel/drilling_riser/stackup.py</code>: top_tension_required, effective_tension,
wall_thickness_required, minimum_slip_ring_tension and inherited defaults.</li>
<li><code>src/digitalmodel/drilling_riser/workflow.py</code>: router, depth-grid validation and optional slip-ring input contract.</li>
<li><code>src/digitalmodel/drilling_riser/assembly.py</code>: separate tensioner capacity and survival calculation.</li>
<li><code>src/digitalmodel/drilling_riser/adapter.py</code>: force, length and pressure conversion constants.</li>
<li><code>src/digitalmodel/drilling_riser/docs/agent-pack.md</code>: package-wide workflow map and evidence boundaries.</li>
<li><code>tests/drilling_riser/fixtures/drive_off_worked_example_2017.json</code>: incomplete published-case transcription and explicit modelling gaps.</li>
<li><code>docs/api/cfd/turbulent-flat-plate-verification.html</code> and <code>docs/api/_assets/brand.css</code>:
existing report layout and canonical stylesheet, inlined here. The required document body class is applied to that existing layout.</li>
<li><code>docs/reports/modules/drillingriser/build_report.py</code>: report calculation and deterministic rendering.</li></ul>
{inventory()}
</section>
<section id="acronyms"><h2 id="acronyms-heading">10. Acronyms</h2>
<p>Abbreviations used in the calculation records are defined here. Symbols used in the equations are defined in Methodology.</p>
<table><thead><tr><th>Term</th><th>Definition</th></tr></thead><tbody>
<tr><td>CSV</td><td>Comma-separated values.</td></tr><tr><td>HTML</td><td>Hypertext Markup Language.</td></tr>
<tr><td>kip</td><td>Thousand pounds-force.</td></tr><tr><td>psi</td><td>Pounds-force per square inch.</td></tr>
<tr><td>kN</td><td>Kilonewton.</td></tr><tr><td>MPa</td><td>Megapascal.</td></tr>
<tr><td>NOT_EVALUATED</td><td>The result is not evaluated because the required data or analysis is absent.</td></tr>
</tbody></table>
</section>
<aside id="appendix"><h3>Appendix: native inputs and calculated profile</h3>
<p>The native input register preserves the module units. Each value is read from the public example;
no engineering case value is typed into the report text.</p>
{input_table(data, native=True)}
<table><thead><tr><th>Depth fraction (%)</th><th>Effective force (kip)</th><th>Source</th></tr></thead>
<tbody>{profile_rows}</tbody><caption>Calculated profile from the router CSV.</caption></table>
<h3>Appendix: data and assumption register</h3>
<table><thead><tr><th>Item</th><th>Status and basis</th><th>Consequence</th></tr></thead><tbody>
<tr><td>Case scalars and multipliers</td><td>ASSUMED - to be confirmed; retained from the public example.</td><td>Illustrative sizing only.</td></tr>
<tr><td>Uniform weight distribution</td><td>ASSUMED - to be confirmed; linear profile implemented in the module.</td><td>No local component or foam-zone force resolution.</td></tr>
<tr><td>Weight convention</td><td>NOT_EVALUATED; flooded-steel and fluid-head reconciliation is absent.</td><td>Slip-ring value is not a tensioner setting.</td></tr>
<tr><td>Wall thickness and allowances</td><td>NOT_EVALUATED; actual wall, corrosion, wear and tolerances are absent.</td><td>No wall acceptance result.</td></tr>
<tr><td>Adjacent components and added mass</td><td>NOT_EVALUATED; diameters, stiffness ratios, auxiliary lines and hydrodynamic coefficients are absent.</td><td>No stiffness transition or dynamic model.</td></tr>
<tr><td>Host and environment</td><td>NOT_EVALUATED; host-motion data and current- and wave-dominated sets are absent.</td><td>No operating envelope.</td></tr>
<tr><td>Watch circle and disconnect</td><td>NOT_EVALUATED; equipment criteria, disconnect time and closure curves are absent.</td><td>Do not connect on this basis.</td></tr>
</tbody><caption>Data held and not held; no missing equipment value is guessed.</caption></table>
<h3>Appendix: quality assurance pre-flight</h3>
<p>The router is called with the full example, and temporary CSV output is read back for the report.
Top force, wall estimate, slip-ring force and every profile station are reconciled to direct module calls in the rebuild tests.
Independent decimal arithmetic checks equilibrium and the pressure relation. A changed-input test checks propagation
through the summary, chart and displayed values. Byte-for-byte rebuilding checks the delivered HTML against its generator.</p>
<p>The stylesheet, chart data and interaction code are embedded; no external script, image or stylesheet is needed.
Test-definition counts identify evidence held, not a whole-package pass rate. No licensed solver or physical validation is used.</p>
</aside>
</main>
<aside id="comments"><h3>Review comments</h3>
<p>Select report text or a section, enter a comment, then save the comments beside the report.
Comments stay in memory until saved. Folder saving requires a supporting browser and selection of the report folder;
otherwise download the comments and choose that location. Load the saved file to continue reviewing.</p>
<label for="comment-section">Section</label><select id="comment-section">
<option value="summary">Executive Summary</option><option value="intro">Introduction</option>
<option value="data">Design Data</option><option value="method">Analysis Methodology</option>
<option value="results">Detailed Results</option><option value="findings">Key Findings</option>
<option value="limits">Limitations</option><option value="recommendations">Recommendations</option>
<option value="references">References</option><option value="acronyms">Acronyms</option></select>
<p id="comment-context">Executive Summary</p><label for="comment-text">Comment</label>
<textarea id="comment-text" rows="4" maxlength="5000" style="width:100%"></textarea>
<button id="add-comment" type="button">Add comment</button>
<button id="save-comments" type="button">Save beside report</button>
<button id="download-comments" type="button">Download comments</button>
<label for="load-comments">Load saved comments</label><input id="load-comments" type="file" accept="application/json">
<p id="comment-status" role="status">No comments saved.</p><div id="comment-list"></div></aside>
"""
    payload = json.dumps(chart_data(data), ensure_ascii=False, allow_nan=False)
    changes = json.dumps(data["changes"], ensure_ascii=False, allow_nan=False)
    return (f'<!doctype html>\n<html lang="en" data-theme="light"><head>\n<meta charset="utf-8">\n'
            '<meta name="viewport" content="width=device-width, initial-scale=1">\n'
            '<title>Drilling riser stack-up analysis</title>\n'
            f'<style>{styles()}</style>\n</head><body class="hs-doc"><div class="wrap">\n'
            + body + f'\n<script type="application/json" id="engineering-data">{payload}</script>\n'
            + f'<script type="application/json" id="change-data">{changes}</script>\n'
            + INTERACTION + '\n</div></body></html>\n')


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--scratch", type=Path, required=True)
    parser.add_argument("--output", type=Path, default=HERE / "index.html")
    args = parser.parse_args()
    args.output.write_text(render(compute(args.scratch)), encoding="utf-8")


if __name__ == "__main__":
    main()
