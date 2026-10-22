# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Rebuild the standalone geotechnical module analysis from the public example.

Usage: PYTHONPATH=src uv run --no-project --with pyyaml python
    docs/reports/modules/geotechnical/build_report.py --scratch <temporary-folder>

The scratch argument is required. Workflow sidecars stay there; only the HTML
and engineering change record are written to the output directory.
"""

from __future__ import annotations

import argparse
import ast
from copy import deepcopy
from dataclasses import asdict
from html import escape
import inspect
import json
from pathlib import Path
import re

import yaml

from digitalmodel.geotechnical.pile_capacity import alpha_method_capacity
from digitalmodel.geotechnical.piles import skin_friction_clay
from digitalmodel.geotechnical.workflow import GeotechnicalWorkflow


ROOT = Path(__file__).resolve().parents[4]
HERE = Path(__file__).resolve().parent
EXAMPLE = "examples/workflows/pile-axial-capacity/input.yml"
STYLE_EXEMPLAR = "docs/api/cfd/turbulent-flat-plate-verification.html"
EVIDENCE = {
    "Pile primitives": "tests/test_geotechnical_piles.py",
    "Composite clay pile": "tests/geotechnical/test_pile_capacity_composite.py",
    "Input range checks": "tests/geotechnical/test_pile_validation.py",
    "Foundation eccentricity": "tests/geotechnical/test_mudmat_eccentricity.py",
    "Sediment-motion arithmetic": "tests/geotechnical/test_sediment_motion.py",
    "Optional slope benchmarks": "tests/geotechnical/test_slope_stability.py",
}
CHANGES = {
    "revision": "A",
    "date": "2026-10-22",
    "changes": [{
        "section": "worked-example",
        "description": "Axial clay-pile resistance and the illustrative load boundary are documented, with the full-area tip assumption and qualification gaps exposed.",
        "reason": "Separate reproducible calculation behaviour from design acceptance.",
    }],
}


def method_parameters() -> dict[str, float]:
    """Read displayed alpha coefficients from the implemented expression tree."""
    tree = ast.parse(inspect.getsource(skin_friction_clay))
    branch = next(node for node in ast.walk(tree) if isinstance(node, ast.If)
                  and isinstance(node.test, ast.Compare))
    lower = branch.body[0].value
    upper = branch.orelse[0].value
    cap = next(node.value for node in ast.walk(tree) if isinstance(node, ast.Assign)
               and isinstance(node.value, ast.Call) and isinstance(node.value.func, ast.Name)
               and node.value.func.id == "min")
    return {
        "lower_coefficient": ast.literal_eval(lower.left),
        "lower_exponent": ast.literal_eval(lower.right.right),
        "upper_coefficient": ast.literal_eval(upper.left),
        "upper_exponent": ast.literal_eval(upper.right.right),
        "branch_ratio": ast.literal_eval(branch.test.comparators[0]),
        "alpha_cap": ast.literal_eval(cap.args[1]),
    }


def calculate(scratch: Path) -> dict:
    """Call the documented pile function and workflow; retain only public data."""
    scratch.mkdir(parents=True, exist_ok=True)
    fixture = yaml.safe_load((ROOT / EXAMPLE).read_text(encoding="utf-8"))
    case = fixture["geotechnical"]["pile_axial_capacity"]
    inputs = dict(D=case["pile"]["diameter_m"], L=case["pile"]["embedded_length_m"],
                  Su=case["soil"]["Su_kpa"], sigma_v=case["soil"]["sigma_v_kpa"],
                  Nc=case["pile"]["Nc"])
    result = {key: value for key, value in asdict(alpha_method_capacity(**inputs)).items()
              if isinstance(value, float)}

    def point(length: float, name: str) -> dict:
        cfg = deepcopy(fixture)
        params = cfg["geotechnical"]["pile_axial_capacity"]
        params["pile"]["embedded_length_m"] = length
        params["outputs"]["directory"] = str(scratch / name)
        output = GeotechnicalWorkflow().router(cfg)["geotechnical"]["pile_axial_capacity"]
        design = output["design"]
        return dict(length_m=length, allowable_capacity_kn=design["allowable_capacity_kn"],
                    demand_kn=design["applied_load_kn"], utilization=design["utilization"],
                    factor_of_safety=design["factor_of_safety"], status=design["status"])

    # Sampling is a plotting choice, not an extension of the soil investigation:
    # half to one-and-a-half times the example length, with equal increments.
    sweep = [point(inputs["L"] * (0.5 + index / 20), f"sample-{index}")
             for index in range(21)]
    inventory = {}
    for name, path in EVIDENCE.items():
        tree = ast.parse((ROOT / path).read_text(encoding="utf-8"))
        inventory[name] = sum(isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef))
                              and node.name.startswith("test_") for node in ast.walk(tree))
    return dict(inputs=inputs, result=result, design=point(inputs["L"], "example"),
                sweep=sweep, method=method_parameters(), inventory=inventory)


def quantity(data: dict, key: str, source: str = "calculator", precision: int = 1) -> str:
    """Bind displayed engineering numbers to their unrounded calculation field."""
    value = data
    for part in key.split("."):
        value = value[int(part)] if isinstance(value, list) else value[part]
    printed = f"{value:,.{precision}f}".replace("-", "−")
    return (f'<span data-quantity="{key}" data-value="{value}" '
            f'data-source="{source}" data-precision="{precision}">{printed}</span>')


def schematic(data: dict) -> str:
    """Draw the calculation idealisation without implying a wall thickness."""
    q = lambda key: quantity(data, key, "example")
    return f'''<figure>
<svg viewBox="0 0 760 450" role="img" aria-labelledby="pile-title pile-description">
<title id="pile-title">Axially loaded pile in a uniform clay idealisation</title>
<desc id="pile-description">A cylindrical pile transfers a downward axial load into upward shaft resistance and upward tip resistance. Diameter is shown in plan. Wall thickness and plugging are unspecified; a full circular tip area is used in the calculation. The vertical drawing is compressed.</desc>
<defs><linearGradient id="steel"><stop stop-color="#bdc7d1"/><stop offset=".5" stop-color="#edf1f5"/><stop offset="1" stop-color="#8c9caa"/></linearGradient>
<pattern id="clay" width="22" height="18" patternUnits="userSpaceOnUse"><path d="M2 6h9m3 8h7" stroke="#b8a88c"/></pattern>
<marker id="arrow" markerWidth="7" markerHeight="7" refX="5" refY="3" orient="auto-start-reverse"><path d="M0 0L6 3L0 6Z" fill="currentColor"/></marker></defs>
<path d="M25 125H735V410H25Z" fill="#eae1d1"/><path d="M25 125H735V410H25Z" fill="url(#clay)"/>
<path d="M25 125H735" stroke="currentColor"/><text x="35" y="113">Soil surface</text>
<path d="M320 65V370Q350 386 380 370V65" fill="url(#steel)" stroke="currentColor"/>
<ellipse cx="350" cy="65" rx="30" ry="9" fill="#b4c0cb" stroke="currentColor"/>
<path d="M350 12V48" marker-end="url(#arrow)" stroke="currentColor"/>
<text x="405" y="30">Axial compression P</text>
<path d="M295 280V220M405 280V220M350 425V390" marker-end="url(#arrow)" stroke="currentColor"/>
<text x="65" y="255">Shaft resistance Qₛ</text><text x="410" y="419">Tip resistance Qᵦ</text>
<path d="M455 128V370" marker-start="url(#arrow)" marker-end="url(#arrow)" stroke="currentColor"/>
<text x="468" y="205">Embedded length L</text>
<circle cx="620" cy="72" r="30" fill="url(#steel)" stroke="currentColor"/>
<path d="M590 72H650" marker-start="url(#arrow)" marker-end="url(#arrow)" stroke="currentColor"/>
<text x="560" y="20">Plan: diameter D</text>
<text x="65" y="335">Uniform undrained clay</text><text x="65" y="357">sᵤ and σ′ᵥ supplied</text>
</svg>
<figcaption>Calculation idealisation. D = {q("inputs.D")} m and L = {q("inputs.L")} m.
The vertical scale is compressed; the full circular tip area is an arithmetic assumption, not a plugging assessment. <a href="#source-example">Public example</a>; <a href="#source-calculator">calculation basis</a>.</figcaption></figure>'''


def chart(data: dict) -> str:
    """Render an offline interactive capacity curve and the applied-load limit."""
    points = data["sweep"]
    xmin, xmax = points[0]["length_m"], points[-1]["length_m"]
    ymax = max(p["allowable_capacity_kn"] for p in points) * 1.15
    x = lambda value: 80 + (value - xmin) / (xmax - xmin) * 570
    y = lambda value: 320 - value / ymax * 270
    circles = []
    for index, point in enumerate(points):
        status = "The illustrative load check passes." if point["status"] == "PASS" else "The illustrative load check fails."
        label = (f'Case: length {point["length_m"]:.1f} m; Component: total allowable axial resistance; '
                 f'demand {point["demand_kn"]:.1f} kN; limit {point["allowable_capacity_kn"]:.1f} kN. {status}')
        circles.append(f'<circle class="case-point" data-index="{index}" tabindex="0" role="button" '
                       f'aria-label="{label}" cx="{x(point["length_m"]):.2f}" '
                       f'cy="{y(point["allowable_capacity_kn"]):.2f}" r="5" '
                       f'fill="var(--{"go" if point["status"] == "PASS" else "nogo"})"><title>{label}</title></circle>')
    line = " ".join(f'{x(p["length_m"]):.2f},{y(p["allowable_capacity_kn"]):.2f}' for p in points)
    limit_y = y(points[0]["demand_kn"])
    return f'''<figure>
<svg id="capacity-chart" viewBox="0 0 760 390" role="img" aria-labelledby="chart-title chart-description">
<title id="chart-title">Allowable axial resistance against embedded length</title>
<desc id="chart-description">Calculated length sensitivity with the example soil held constant. The applied load is drawn as a dashed limit. Each point is focusable and provides demand, resistance and the illustrative load verdict.</desc>
<path d="M80 45V320H660" stroke="currentColor" fill="none"/>
<path id="demand-limit" d="M80 {limit_y:.2f}H660" stroke="var(--nogo)" stroke-dasharray="8 5"/>
<text x="85" y="{limit_y - 12:.2f}">Applied load limit</text>
<polyline points="{line}" fill="none" stroke="var(--brand)" stroke-width="2"/>
{"".join(circles)}
<text x="270" y="374">Embedded length (m)</text>
<text x="24" y="290" transform="rotate(-90 24 290)">Allowable axial resistance (kN)</text>
<text x="70" y="343">{xmin:.1f}</text><text x="635" y="343">{xmax:.1f}</text>
<text x="28" y="323">0.0</text><text x="12" y="{limit_y + 5:.2f}">{points[0]["demand_kn"]:,.1f}</text>
</svg>
<label for="case-selector">Select a calculated length</label>
<input id="case-selector" type="range" min="0" max="{len(points) - 1}" step="1" value="{len(points) // 2}">
<p id="case-readout" aria-live="polite"></p>
<figcaption>Length sensitivity is calculated by calling the same workflow at each point.
Green means the illustrative load check passes; red means the illustrative load check fails; amber means design qualification is not evaluated.
The applied-load line is the same load as the public example, not an environmental or settlement limit.
The plotted soil and stress remain constant over the whole sensitivity interval. <a href="#source-example">Public example</a>; <a href="#source-workflow">load criterion</a>.</figcaption></figure>'''


SCRIPT = r'''
<script>
"use strict";
const calculation = JSON.parse(document.getElementById("calculation").textContent);
const changes = JSON.parse(document.getElementById("engineering-changes").textContent);
const selector = document.getElementById("case-selector");
function showCase(index) {
  const point = calculation.sweep[index];
  const status = point.status === "PASS" ? "The illustrative load check passes." : "The illustrative load check fails.";
  document.getElementById("case-readout").textContent =
    `Case: length ${point.length_m.toFixed(1)} m. Component: total allowable axial resistance. ` +
    `Demand: ${point.demand_kn.toFixed(1)} kN. Limit: ${point.allowable_capacity_kn.toFixed(1)} kN. ${status}`;
  selector.value = index;
}
selector.addEventListener("input", () => showCase(Number(selector.value)));
document.querySelectorAll(".case-point").forEach(point => {
  const show = () => showCase(Number(point.dataset.index));
  point.addEventListener("mouseenter", show);
  point.addEventListener("focus", show);
  point.addEventListener("click", show);
  point.addEventListener("keydown", event => {
    if (event.key === "Enter" || event.key === " ") { event.preventDefault(); show(); }
  });
});
showCase(Number(selector.value));
changes.changes.forEach(change => {
  const item = document.createElement("li");
  const link = document.createElement("a");
  link.href = "#" + change.section;
  link.textContent = change.description;
  item.appendChild(link);
  item.appendChild(document.createTextNode(" " + change.reason));
  document.getElementById("changes-list").appendChild(item);
});
let comments = [];
const message = document.getElementById("comment-message");
function renderComments() {
  const list = document.getElementById("comment-list");
  list.replaceChildren();
  comments.forEach(comment => {
    const item = document.createElement("li");
    item.textContent = `${comment.section}: ${comment.text}`;
    list.appendChild(item);
  });
}
document.getElementById("add-comment").addEventListener("click", () => {
  const text = document.getElementById("comment-text").value.trim();
  if (!text) { message.textContent = "Enter a comment before adding it."; return; }
  comments.push({section: document.getElementById("comment-section").value, text});
  document.getElementById("comment-text").value = "";
  renderComments();
  message.textContent = "Comment added in memory. Save the comments before closing this document.";
});
function commentFile() {
  return JSON.stringify({revision: changes.revision, comments}, null, 2) + "\n";
}
document.getElementById("save-comments").addEventListener("click", async () => {
  try {
    if (!window.showDirectoryPicker) {
      message.textContent = "Folder saving is unavailable. Use Download comments and place the file beside this report.";
      return;
    }
    const directory = await window.showDirectoryPicker({mode: "readwrite"});
    const handle = await directory.getFileHandle("index.comments.json", {create: true});
    const writable = await handle.createWritable();
    await writable.write(commentFile());
    await writable.close();
    message.textContent = "Comments saved as index.comments.json in the selected report folder.";
  } catch (error) {
    message.textContent = error.name === "AbortError" ? "Saving cancelled; comments remain in memory." : "Saving failed; use Download comments.";
  }
});
document.getElementById("download-comments").addEventListener("click", () => {
  const url = URL.createObjectURL(new Blob([commentFile()], {type: "application/json"}));
  const link = document.createElement("a");
  link.href = url; link.download = "index.comments.json";
  document.body.appendChild(link); link.click(); link.remove(); URL.revokeObjectURL(url);
  message.textContent = "Comments downloaded. Place index.comments.json beside this report.";
});
document.getElementById("load-comments").addEventListener("change", async event => {
  const file = event.target.files[0];
  if (!file) return;
  try {
    const record = JSON.parse(await file.text());
    const allowed = new Set(Array.from(document.getElementById("comment-section").options).map(option => option.value));
    if (record.revision !== changes.revision || !Array.isArray(record.comments) ||
        !record.comments.every(comment => typeof comment.text === "string" && allowed.has(comment.section))) {
      throw new Error("Invalid comment record");
    }
    comments = record.comments.map(comment => ({section: comment.section, text: comment.text}));
    renderComments(); message.textContent = "Saved comments loaded.";
  } catch (error) { message.textContent = "The comment file is invalid or belongs to another revision. Existing comments are retained."; }
});
</script>
'''


def render(data: dict) -> str:
    """Write the engineering narrative with the full module scope in context."""
    q = lambda key, source="calculator", precision=1: quantity(data, key, source, precision)
    styles = (ROOT / "docs/api/_assets/brand.css").read_text(encoding="utf-8").strip()
    exemplar = (ROOT / STYLE_EXEMPLAR).read_text(encoding="utf-8")
    house = re.search(r"<style>(.*?)</style>", exemplar, re.S)[1]
    headings = [("summary", "1 Executive Summary"), ("introduction", "2 Introduction"),
                ("controls", "3 Engineering summary"), ("basis", "4 Design basis and inputs"),
                ("method", "5 Theory and methodology"), ("worked-example", "6 Worked example"),
                ("evidence", "7 Validation evidence"), ("limits", "8 Limits and recommendations"),
                ("appendix", "9 Appendices"), ("review", "10 Review comments and changes")]
    nav = " · ".join(f'<a href="#{key}">{name}</a>' for key, name in headings)
    options = "".join(f'<option value="{key}">{name}</option>' for key, name in headings)
    inputs = [("D", "Pile outside diameter", "m"), ("L", "Embedded length", "m"),
              ("Su", "Undrained shear strength", "kPa"), ("sigma_v", "Effective vertical stress", "kPa"),
              ("Nc", "Tip bearing factor", "dimensionless")]
    input_rows = "".join(f'<tr><td>{label}</td><td>{q("inputs." + key, "example")}</td><td>{unit}</td>'
                         '<td><a href="#source-example">Public example input</a></td></tr>' for key, label, unit in inputs)
    results = [("alpha", "Adhesion factor α", "dimensionless"),
               ("unit_skin_friction_kpa", "Unit shaft resistance f", "kPa"),
               ("skin_friction_kn", "Shaft resistance Qₛ", "kN"),
               ("end_bearing_kn", "Tip resistance Qᵦ", "kN"),
               ("total_capacity_kn", "Ultimate resistance Qᵤ", "kN")]
    result_rows = "".join(f'<tr><td>{label}</td><td>{q("result." + key)}</td><td>{unit}</td>'
                          '<td><a href="#source-calculator">Calculated result</a></td></tr>' for key, label, unit in results)
    evidence_rows = "".join(f'<tr><td>{escape(name)}</td><td>{count}</td><td><a href="#source-tests">Test inventory</a></td></tr>'
                            for name, count in data["inventory"].items())
    references = [
        ("source-example", EXAMPLE, "Public axial clay-pile example; pile, soil and design blocks. Repository edition accompanying this report. Inputs are illustrative; external provenance is not established."),
        ("source-calculator", "src/digitalmodel/geotechnical/pile_capacity.py", "Public function documentation and single/multilayer assembly: alpha_method_capacity and alpha_method_capacity_multilayer. Repository edition accompanying this report."),
        ("source-alpha", "src/digitalmodel/geotechnical/piles.py", "skin_friction_clay, end_bearing_clay and axial_capacity. Repository implementation is the source of the displayed coefficients; it is not an edition-qualified standard."),
        ("source-workflow", "src/digitalmodel/geotechnical/workflow.py", "GeotechnicalWorkflow.router and _run_pile_axial_capacity: configuration routing, design divisor, load comparison and JSON output. Repository edition accompanying this report."),
        ("source-tests", "tests/geotechnical/test_pile_capacity_composite.py", "Component identity, single/multilayer equivalence, tip-layer selection and fixed arithmetic assertions. Test-function counts below exclude parameter expansion and are not execution counts."),
        ("source-mudmat", "tests/geotechnical/test_mudmat_eccentricity.py", "Independent rectangular-area arithmetic and axis-selection regressions; no soil-bearing qualification is implied."),
        ("source-slope", "docs/domains/geotechnical/slope-stability.md", "Optional slope and steady-seepage interface, upstream benchmark references, version restrictions and unsupported analyses. No slope result is recomputed in this report."),
        ("source-sediment", "tests/geotechnical/test_sediment_motion.py", "Independent threshold arithmetic and finite/positive input tests. USACE HEC-RAS 2D Sediment Technical Reference Manual, version 6.4.1 (2023), sections 2.14.1 and 2.13.1 is cited by the existing test; the external document is not reopened here."),
        ("source-pack", "src/digitalmodel/geotechnical/docs/agent-pack.md", "Module evidence inventory and unresolved source-qualification limits. Historical execution statements are not presented as current test results."),
        ("source-style", STYLE_EXEMPLAR, "Existing engineering report layout and stylesheet exemplar; the canonical brand stylesheet is inlined without alteration."),
    ]
    source_items = "".join(f'<li id="{key}"><a href="../../../../{path}">{path}</a>. {description}</li>'
                           for key, path, description in references)
    test_items = "".join(f'<li><a href="../../../../{path}">{path}</a>: {data["inventory"][name]} test functions.</li>' for name, path in EVIDENCE.items())
    return f'''<!DOCTYPE html>
<html lang="en" data-theme="light"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Geotechnical module analysis — axial clay-pile resistance</title>
<style>{styles}</style>
<style>{house}</style>
</head><body class="hs-doc"><main class="wrap">
<header><div class="kicker">Engineering module analysis</div>
<h1>Geotechnical module analysis</h1><p class="sub">Axial clay-pile resistance and an illustrative load check</p>
<p>Draft for client review</p></header>
<nav aria-label="Report sections">{nav}</nav>

<h2 id="summary">1 Executive Summary</h2>
<p>The geotechnical module is a collection of soil-resistance and screening calculators. Analysis is performed to determine axial clay-pile resistance and the margin against an illustrative applied load; key results are summarized below.</p>
<ul><li>The public example passes its illustrative axial load check.
<ul><li>The allowable axial resistance is {q("design.allowable_capacity_kn", "workflow")} kN against {q("design.demand_kn", "workflow")} kN applied load, using the example design divisor of {q("design.factor_of_safety", "workflow")}. The stated load is the acceptance boundary for this illustration. <a href="#source-example">Input basis</a>; <a href="#source-workflow">criterion</a>.</li>
<li>The full circular tip area is used. Plugging and an independent pile load-test comparison are not evaluated. <a href="#source-calculator">Calculation basis</a>; <a href="#source-pack">qualification record</a>.</li></ul></li>
<li>The calculation is available for reproducible screening.
<ul><li>Design release is withheld until the soil profile, tip condition, method edition and design factors are qualified. The numerical load check alone does not establish design acceptance. <a href="#limits">Limits and recommendations</a>.</li></ul></li></ul>
<h3>Revision history</h3>
<table><thead><tr><th>Revision</th><th>Date</th><th>Change and reason</th></tr></thead><tbody>
<tr><td>{CHANGES["revision"]}</td><td>{CHANGES["date"]}</td><td>Document the public pile calculation, its load boundary and qualification gaps to support engineering use.</td></tr></tbody></table>

<h2 id="introduction">2 Introduction</h2>
<p>The main axial clay-pile workflow is described from input selection through the returned capacities and load verdict. A documented public repository example is replayed by calling the module; it is not represented as a published field benchmark. <a href="#source-example">Example record</a>.</p>
<p>Pile primitives and composite clay-pile functions are provided alongside anchor, foundation, scour, sediment-motion, liquefaction, parameter-database and optional slope interfaces. The interfaces are assessed as software entry points, rather than as a common design certification. <a href="#source-pack">Scope record</a>; <a href="#source-slope">slope interface</a>.</p>

<h2 id="controls">3 Engineering summary</h2>
<p>The applied load is compared with ultimate resistance divided by the supplied design factor. Sensitivity to embedded length is evaluated with diameter, soil strength, effective stress and design factor held at the public example values. <a href="#source-workflow">Criterion and implementation</a>.</p>
{chart(data)}
<p>The failing shorter-length cases are arithmetic sensitivity cases. For the illustrative check, the load is limited to the calculated allowable resistance; design use remains conditional on qualification of the physical pile and soil profile. Lengthening is not accepted as a design recommendation without depth-dependent soil evidence. <a href="#source-workflow">Load criterion</a>; <a href="#source-calculator">constant-property model</a>.</p>

<h2 id="basis">4 Design basis and inputs</h2>
<p>A uniform undrained clay idealisation and concentric compressive loading are adopted from the public example. The source calls the pile open-ended, while the calculator uses the full circular tip area; this discrepancy is retained explicitly as a limitation. <a href="#source-example">Geometry description</a>; <a href="#source-calculator">tip-area expression</a>.</p>
{schematic(data)}
<p>The design factor is treated as an example input, not a prescribed code factor. Load acceptance means applied load does not exceed the calculated allowable resistance; equality is accepted by the workflow. Geometry, strength, effective stress, bearing factor, design factor and demand are required engineering inputs for a qualified use. <a href="#source-workflow">Load comparison</a>.</p>
<p>The public example inputs are marked ASSUMED - to be confirmed for any physical application. They are not measured site properties. No property is selected from the parameter database in this calculation; the data register is provided in the appendix. <a href="#data-register">Input qualification register</a>.</p>

<h2 id="method">5 Theory and methodology</h2>
<p>Shaft adhesion and tip bearing are summed in the public composite pile function. The following equations describe the implementation being replayed; normative attribution and the applicable edition of American Petroleum Institute Recommended Practice 2GEO (API RP 2GEO) remain NOT_EVALUATED. No coefficient is asserted as a qualified standard value. <a href="#source-calculator">Composite function</a>; <a href="#source-alpha">coefficient source</a>.</p>
<div class="eq">ψ = sᵤ / σ′ᵥ; f = α sᵤ; qᵦ = N꜀ sᵤ<br>
α = min(α<sub>cap</sub>, a ψ<sup>b</sup>)<br>
ψ ≤ {q("method.branch_ratio")}: a = {q("method.lower_coefficient")}, b = {q("method.lower_exponent")}<br>
ψ &gt; {q("method.branch_ratio")}: a = {q("method.upper_coefficient")}, b = {q("method.upper_exponent", precision=2)}<br>
α<sub>cap</sub> = {q("method.alpha_cap")}. <a href="#source-alpha">Implemented alpha expression</a>.</div>
<p>Here ψ is the dimensionless strength-to-effective-stress ratio; sᵤ is undrained shear strength and σ′ᵥ is effective vertical stress, both in kPa. The symbols a, b and α<sub>cap</sub> denote the implemented coefficient, exponent and adhesion cap, and α is dimensionless adhesion. The exact upper exponent is the negative quarter-power in the cited implementation. N꜀ is the supplied dimensionless tip bearing factor, f is unit shaft resistance and qᵦ is tip pressure in kPa. <a href="#source-alpha">Definitions and expressions</a>.</p>
<div class="eq">Aₛ = πDL; Aᵦ = πD²/4; Qₛ = f Aₛ; Qᵦ = qᵦ Aᵦ<br>
Qᵤ = Qₛ + Qᵦ; Q<sub>allow</sub> = Qᵤ/F; U = P/Q<sub>allow</sub>.
<a href="#source-calculator">Geometry and resistance</a>; <a href="#source-workflow">design division</a>.</div>
<p>D and L are pile outside diameter and embedded length in metres. Aₛ and Aᵦ are shaft and full circular tip areas in square metres; π is the circle constant. Qₛ, Qᵦ, Qᵤ and Q<sub>allow</sub> are shaft, tip, ultimate and allowable resistance in kN. F is the supplied design divisor, P is applied axial compression in kN, and U is dimensionless load utilization. Acceptance is P ≤ Q<sub>allow</sub>. <a href="#source-calculator">Resistance definitions</a>; <a href="#source-workflow">acceptance expression</a>.</p>
<p>For layered clay, shaft resistance is summed over the supplied layer thicknesses and the final layer supplies tip strength. Returned adhesion and unit shaft resistance refer to the final layer. Layer thicknesses are not reconciled with a separate embedded-length field by the composite function. <a href="#source-calculator">Multilayer interface</a>.</p>

<h2 id="worked-example">6 Worked example</h2>
<p>The public example is evaluated by the documented composite function and by its configuration workflow. All input and result quantities below are inserted from those calls and the example record. <a href="#source-example">Input record</a>; <a href="#source-calculator">public function</a>; <a href="#source-workflow">workflow</a>.</p>
<table><thead><tr><th>Input</th><th>Value</th><th>Unit</th><th>Source</th></tr></thead><tbody>{input_rows}</tbody></table>
<table><thead><tr><th>Output</th><th>Value</th><th>Unit</th><th>Source</th></tr></thead><tbody>{result_rows}</tbody></table>
<p>At length {q("design.length_m", "workflow")} m, the allowable resistance is {q("design.allowable_capacity_kn", "workflow")} kN after division by {q("design.factor_of_safety", "workflow")}. The applied load is {q("design.demand_kn", "workflow")} kN and utilization is {q("design.utilization", "workflow")}. The illustrative load check passes. <a href="#source-workflow">Design calculation</a>.</p>
<p>Inputs are supplied in the example configuration under the pile, soil and design blocks. The public function returns a result dataclass containing the quantities above and a method tag. The workflow accepts the configuration mapping, adds the design quantities and status, and writes a JavaScript Object Notation (JSON) summary to the configured output directory. The return mapping also contains the summary location; that location is added after the file is written. <a href="#source-calculator">Function contract</a>; <a href="#source-workflow">output contract</a>.</p>

<h2 id="evidence">7 Validation evidence</h2>
<p>Arithmetic regressions, component identities, input-domain checks and optional public slope benchmark reproductions are held in the repository. These evidence classes are distinguished because replaying a formula does not qualify its soil model or establish field performance. <a href="#source-tests">Pile tests</a>; <a href="#source-pack">evidence inventory</a>.</p>
<p>For piles, component summation, single/multilayer equivalence, final-layer tip selection and response to changed dimensions are tested. Existing fixed-value assertions are retained as regressions; their external numerical provenance is not established here and their reference values are not reproduced. An independent pile load-test comparison is NOT_EVALUATED. <a href="#source-tests">Existing pile assertions</a>.</p>
<p>Independent rectangular-area checks are held for foundation eccentricity. Sediment-motion tests hold independent arithmetic checks of threshold stress and friction velocity. Neither validates the pile calculation. Public slope workbooks and optional reproduction tests are documented separately; slope results are not recalculated for this report. <a href="#source-mudmat">Area checks</a>; <a href="#source-sediment">threshold checks</a>; <a href="#source-slope">public benchmark record</a>.</p>
<p>The report rebuild test compares every result field with a fresh public-function call and every plotted case with a fresh workflow call. Printed engineering values are checked against their unrounded calculation fields. Byte equality is required for the rebuilt report and change record. The appendix inventory counts source test functions, rather than executed parameterized cases. <a href="#source-rebuild">Rebuild contract</a>.</p>

<h2 id="limits">8 Limits and recommendations</h2>
<p>Design acceptance is not assigned beyond the illustrative load inequality. The following gaps are material to physical pile resistance and are not resolved by a passing software regression. <a href="#source-calculator">Model boundary</a>; <a href="#source-pack">qualification record</a>.</p>
<h3>8.1 Recommendations and limits of use</h3>
<p>The full-area tip result is conditional on the model idealisation; wall thickness, annular bearing and plugging are not represented. Before design release, establish the applicable tip condition and compare an appropriate tip model with independent evidence. For a failing load check, limit applied compression to the calculated allowable value while the physical model is qualified. <a href="#source-calculator">Tip formulation</a>; <a href="#source-workflow">load criterion</a>.</p>
<p>Strength and effective stress are constant within each supplied layer; the calculation does not derive a stress or strength profile. Before extending embedment, establish depth-dependent soil properties and reconcile the supplied layer thicknesses with geometry. Settlement, installation effects, cyclic degradation, tension, lateral response and group interaction require separate assessments. <a href="#source-calculator">Supported inputs and equations</a>.</p>
<p>The composite entry point does not enforce a complete finite, positive input domain. Validate dimensions, stresses, strengths, layer order and the positive design divisor before use; the optional range checker in the lower-level pile interface is not a site-specific validity envelope. Before design release, record the selected standard edition, formula attribution and independent acceptance evidence. <a href="#source-alpha">Input validation</a>; <a href="#source-calculator">composite contract</a>.</p>
<h3>8.2 Not covered</h3>
<p>Sand piles, drag and suction anchors, mudmat bearing and sliding, legacy pipeline and monopile scour, armour sizing, liquefaction, sediment-motion thresholds, database property selection and optional slope/seepage calculations are outside the worked example. The legacy scour attribution is not qualified by the separately sourced sediment-motion helper. Future calculations require an explicit method basis, qualified inputs and independent reference cases for each sub-area. <a href="#source-pack">Module boundaries</a>; <a href="#source-sediment">threshold scope</a>; <a href="#source-slope">slope boundaries</a>.</p>

<h2 id="appendix">9 Appendices</h2>
<p>The input qualification register and reproducibility evidence are retained here. Implementation locators and source editions are collected in the source register so that the main calculation remains readable.</p>
<h3 id="data-register">9.1 Data register and QA pre-flight</h3>
<table><thead><tr><th>Data group</th><th>Qualification</th><th>Required confirmation</th><th>Source</th></tr></thead><tbody>
<tr><td>Diameter and embedded length</td><td>ASSUMED - to be confirmed</td><td>Physical geometry and embedment</td><td><a href="#source-example">Public example</a></td></tr>
<tr><td>Strength and effective stress</td><td>ASSUMED - to be confirmed</td><td>Site profile and appropriate drainage basis</td><td><a href="#source-example">Public example</a></td></tr>
<tr><td>Tip factor and design divisor</td><td>ASSUMED - to be confirmed</td><td>Method edition, load basis and acceptance factor</td><td><a href="#source-example">Public example</a></td></tr>
<tr><td>Applied load</td><td>ASSUMED - to be confirmed</td><td>Governing load combination</td><td><a href="#source-example">Public example</a></td></tr>
<tr><td>Plugging, wall thickness and field comparison</td><td>NOT_EVALUATED</td><td>Required input and independent evidence; no value is inferred</td><td><a href="#source-calculator">Model boundary</a></td></tr>
</tbody></table>
<table><thead><tr><th>Evidence group</th><th>Test functions</th><th>Source</th></tr></thead><tbody>{evidence_rows}</tbody></table>
<p>Quality assurance (QA pre-flight) is defined by the attached rebuild test: standalone resources, unique anchors, linked source records, numeric bindings, result equality, chart point equality, demand line, house stylesheet reuse and change/comment controls are checked. Browser rendering and folder-write behaviour require reader confirmation in the review controls; automated checks do not claim browser compatibility. <a href="#source-rebuild">Test contract</a>.</p>
<h3>9.2 Sources and locators</h3>
<ol>{source_items}
<li id="source-rebuild"><a href="../../../../tests/geotechnical/test_module_report.py">tests/geotechnical/test_module_report.py</a>. Rebuild, numeric and document checks for this report. <a href="build_report.py">build_report.py</a> calls the module; the scratch directory is required. Rebuild with the repository source directory on the Python import path, PyYAML available, and the builder's scratch option pointing to a temporary directory.</li></ol>
<p>The public repository example is the numerical example source, not an independently published design calculation. No licensed publication was opened and no value is quoted from one. External manual retrieval did not supply usable evidence; standard-edition and clause qualification remain NOT_EVALUATED. Source test-function inventory:</p><ul>{test_items}</ul>

<h2 id="review">10 Review comments and changes</h2>
<p>Engineering changes are listed for this revision. Section comments are retained in a separate JSON file beside the report, so that calculation content remains intact.</p>
<details id="changes-panel" open><summary>Engineering changes</summary><ul id="changes-list"></ul></details>
<details id="comments-panel" open><summary>Section comments</summary>
<p>Select a section, add a comment, and save it. Save comments beside report opens a folder selector; select the folder containing this report. When folder saving is unavailable, use Download comments and place the file beside the report. Load comments reads that file on reopening. Unsaved comments remain only in memory.</p>
<label for="comment-section">Report section</label><select id="comment-section">{options}</select>
<label for="comment-text">Comment</label><textarea id="comment-text" rows="4" style="width:100%"></textarea>
<button id="add-comment" type="button">Add comment</button>
<button id="save-comments" type="button">Save comments beside report</button>
<button id="download-comments" type="button">Download comments</button>
<label for="load-comments">Load comments</label><input id="load-comments" type="file" accept="application/json,.json">
<p id="comment-message" role="status">No comments have been saved.</p><ul id="comment-list"></ul></details>
<noscript>Interactive charts and comments require JavaScript. The input and result tables remain readable.</noscript>
<footer>Geotechnical module analysis. The illustrative load check is conditional on the stated model and inputs.</footer>
</main>
<script id="calculation" type="application/json">{json.dumps(data, ensure_ascii=False, allow_nan=False)}</script>
<script id="engineering-changes" type="application/json">{json.dumps(CHANGES, ensure_ascii=False)}</script>
{SCRIPT}</body></html>
'''


def build(output: Path, scratch: Path) -> None:
    """Write the deterministic HTML and companion engineering change record."""
    data = calculate(scratch)
    output.mkdir(parents=True, exist_ok=True)
    (output / "index.html").write_text(render(data), encoding="utf-8")
    (output / "changes.json").write_text(json.dumps(CHANGES, indent=2) + "\n", encoding="utf-8")


def main() -> None:
    """Require an explicit location for calculation sidecars."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--scratch", type=Path, required=True)
    parser.add_argument("--output", type=Path, default=HERE)
    args = parser.parse_args()
    build(args.output, args.scratch)


if __name__ == "__main__":
    main()
