#!/usr/bin/env python3
# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Rebuild the code checks module analysis from the published offline example.

Run with PYTHONPATH=src python scripts/reporting/build_codechecks_module_report.py
--output DIRECTORY. The calculation CSV is disposable; only HTML and its change
panel are delivered. No network, external JavaScript or solver is used.
"""

from __future__ import annotations

import argparse
import ast
import copy
import hashlib
import html
import json
import os
from pathlib import Path
import re
import tempfile
from typing import Any

import yaml

from digitalmodel.code_checks.workflow import router

ROOT = Path(__file__).resolve().parents[2]
EXAMPLE = "examples/workflows/api-rp-2rd-riser/input.yml"
CHECKER = "src/digitalmodel/orcaflex/code_check_engine.py"
STYLE = "src/digitalmodel/reporting/assets/calc_report_format.html"
TEST_FILES = (
    "tests/code_checks/test_code_check_workflow.py",
    "tests/code_checks/test_dnv_os_f101.py",
    "tests/code_checks/test_one_foot.py",
    "tests/code_checks/test_shell_report_example.py",
    "tests/orcaflex/test_code_check_engine.py",
)
SOURCE_PATHS = (
    EXAMPLE, CHECKER, "src/digitalmodel/code_checks/workflow.py",
)
REFERENCE_PATHS = (
    "src/digitalmodel/code_checks/dnv_os_f101.py",
    "src/digitalmodel/code_checks/storage_tanks/one_foot.py",
    "src/digitalmodel/code_checks/storage_tanks/report.py",
    "docs/registry/workflows.yaml", STYLE,
    "docs/domains/storage-tanks/one-foot-worked-example.html",
    "examples/code_checks/shell_thickness_reports.py", *TEST_FILES,
)
CHANGES: dict[str, Any] = {
    "report": "Code checks module analysis",
    "revision": "01",
    "date": "2026-10-22",
    "changes": [{
        "section": "Calculation evidence and references",
        "description": "Limit fingerprints to the calculation inputs, add an independent arithmetic comparison and clarify the review status and shell-method citation.",
        "reason": "Keep calculation evidence independent of contextual edits and identify the existing equation locator.",
    }],
}
REVISION_HISTORY = [
    ["00", "2026-10-22", "Initial issue: describe the offline riser calculation, reproduce the public example and identify the pressure and qualification limits."],
    [CHANGES["revision"], CHANGES["date"], CHANGES["changes"][0]["description"]],
]


def escape(value: object) -> str:
    """Escape a value before insertion into HTML markup."""
    return html.escape(str(value), quote=True)


def read(path: str) -> str:
    """Read repository evidence with normalized text line endings."""
    return (ROOT / path).read_text(encoding="utf-8")


def calculation() -> dict[str, Any]:
    """Call the public module with every supplied example input, in isolation."""
    config = yaml.safe_load(read(EXAMPLE))
    inputs = copy.deepcopy(config["code_check"]["inputs"])
    # tempfile follows the launch environment; no output enters the example tree.
    with tempfile.TemporaryDirectory(prefix="codechecks-", dir=os.environ.get("TMPDIR")) as scratch:
        config["code_check"]["output_dir"] = scratch
        result = router(config)["code_check"]
        result.pop("results_csv")
    checker = ast.parse(read(CHECKER))
    function = next(n for n in checker.body if isinstance(n, ast.FunctionDef) and n.name == "check_api_rp_2rd")
    limits = [n.comparators[0].value for n in ast.walk(function)
              if isinstance(n, ast.Compare) and isinstance(n.left, ast.Name)
              and n.left.id == "util" and isinstance(n.ops[0], ast.LtE)
              and isinstance(n.comparators[0], ast.Constant)]
    if len(limits) != 1:
        raise ValueError("The implemented utilisation comparison needs reassessment")
    sources = [{"path": p, "sha256": hashlib.sha256(read(p).encode("utf-8")).hexdigest()}
               for p in SOURCE_PATHS]
    tests = [{"path": p, "definitions": sum(isinstance(n, (ast.FunctionDef, ast.AsyncFunctionDef))
              and n.name.startswith("test_") for n in ast.walk(ast.parse(read(p))))}
             for p in TEST_FILES]
    return {"inputs": inputs, "result": result, "limit": limits[0],
            "sources": sources, "test_inventory": tests, "changes": CHANGES,
            "chart": {"arc_length": [r["arc_length"] for r in result["results"]],
                      "utilisation": [r["utilisation"] for r in result["results"]],
                      "limit": limits[0]}}


def number(data: dict[str, Any], key: str, fmt: str = ".1f", scale: float = 1) -> str:
    """Render an auditable scalar, with an explicit optional unit conversion."""
    value: Any = data
    for part in key.split("."):
        value = value[int(part)] if isinstance(value, list) else value[part]
    return (f'<span class="num" data-field="{escape(key)}" data-value="{value}" '
            f'data-format="{fmt}" data-scale="{scale}">{format(value * scale, fmt)}</span>')


def table(headers: list[str], rows: list[list[str]], caption: str, table_id: str = "") -> str:
    """Render real table columns with a caption beneath the table."""
    identity = f' id="{table_id}"' if table_id else ""
    return (f'<div class="tbl-wrap"><table{identity}><thead><tr>'
            + "".join(f"<th>{h}</th>" for h in headers) + "</tr></thead><tbody>"
            + "".join("<tr>" + "".join(f"<td>{v}</td>" for v in row) + "</tr>" for row in rows)
            + f'</tbody></table></div><p class="fig-cap">{caption}</p>')


def section(identity: str, title: str, body: str) -> str:
    """Render a section in the existing house layout."""
    return (f'<section id="{identity}"><div class="l1head"><span class="secnum"></span>'
            f'<h2>{title}</h2></div>{body}</section>')


def schematic(data: dict[str, Any]) -> str:
    """Draw a cutaway annular section with the supplied pipe dimensions."""
    diameter = number(data, "inputs.outer_diameter", ".4f")
    thickness = number(data, "inputs.wall_thickness", ".3f", 1000)
    return f'''<figure class="card">
<svg viewBox="0 0 860 300" role="img" aria-labelledby="pipe-title pipe-desc">
<title id="pipe-title">Annular riser pipe and supplied section loads</title>
<desc id="pipe-desc">A cutaway cylindrical steel pipe has an annular wall. Opposed axial arrows represent tension, and a curved arrow represents bending. The coordinate s follows the pipe. No support or global deformation is solved.</desc>
<defs><linearGradient id="steel" x2="0" y2="1"><stop stop-color="#cbd5df"/><stop offset=".5" stop-color="#f0f3f6"/><stop offset="1" stop-color="#8398aa"/></linearGradient>
<marker id="arrow" markerWidth="8" markerHeight="8" refX="6" refY="3" orient="auto"><path d="M0,0 L6,3 L0,6 Z" fill="currentColor"/></marker></defs>
<path d="M260 85 L620 85 A75 65 0 0 1 620 215 L260 215 Z" fill="url(#steel)" stroke="#526675"/>
<ellipse cx="260" cy="150" rx="48" ry="65" fill="#8d9fae" stroke="#526675"/>
<ellipse cx="260" cy="150" rx="35" ry="48" fill="#233d50"/>
<path d="M260 102 L580 102 L580 150 L295 150 A35 48 0 0 0 260 102" fill="#334f64"/>
<path d="M200 150 H110" stroke="#009b84" stroke-width="3" marker-end="url(#arrow)"/>
<path d="M670 150 H765" stroke="#009b84" stroke-width="3" marker-end="url(#arrow)"/>
<path d="M625 60 C690 20 735 65 710 105" fill="none" stroke="#d9820a" stroke-width="3" marker-end="url(#arrow)"/>
<path d="M360 245 H570" stroke="#526675" stroke-width="2" marker-end="url(#arrow)"/>
<text x="115" y="130">Effective tension T</text><text x="700" y="130">T</text>
<text x="655" y="32">Bending moment M</text><text x="380" y="270">Arc coordinate s</text>
<text x="55" y="55">Outside diameter D</text><path d="M185 60 L240 90" stroke="#526675"/>
<text x="45" y="235">Wall thickness t</text><path d="M175 227 L227 191" stroke="#526675"/>
<text x="420" y="190">Steel wall</text><text x="345" y="128" style="fill:#fff">Fluid bore</text>
</svg><figcaption>Figure A. Annular section and section loads; the axial view is schematic. Outside diameter is {diameter} m and wall thickness is {thickness} mm, from the <a href="#source-example">public example</a>. The drawing is not a global riser configuration.</figcaption></figure>'''


def chart(data: dict[str, Any]) -> str:
    """Plot the returned section utilisations with their implementation limit."""
    rows, limit = data["result"]["results"], data["limit"]
    xmax = max(row["arc_length"] for row in rows)
    ymax = max(limit, *(row["utilisation"] for row in rows)) * 1.15
    x = lambda value: 90 + value / xmax * 650
    y = lambda value: 300 - value / ymax * 245
    points = " ".join(f'{x(r["arc_length"]):.3f},{y(r["utilisation"]):.3f}' for r in rows)
    dots = []
    for index, row in enumerate(rows):
        tooltip = (f'Public example; riser section; position {row["arc_length"]:.1f} m; '
                   f'tension {row["tension_kN"]:.1f} kN; bending {row["bending_moment_kNm"]:.1f} kN m; '
                   f'pressure echo {row["pressure_diff_MPa"]:.1f} MPa; demand {row["utilisation"]:.1%}; '
                   f'screen limit {limit:.1%}; {"Screen passes." if row["pass"] else "Screen exceeds limit."}')
        dots.append(f'<circle class="chart-point" cx="{x(row["arc_length"]):.3f}" cy="{y(row["utilisation"]):.3f}" r="7" fill="#009b84" tabindex="0" data-row="{index}" aria-label="{escape(tooltip)}"><title>{escape(tooltip)}</title></circle>')
    ticks = "".join(f'<text x="{x(r["arc_length"]):.3f}" y="322" text-anchor="middle">{r["arc_length"]:.1f}</text>' for r in rows)
    return f'''<figure class="card"><svg viewBox="0 0 860 370" role="group" aria-labelledby="chart-title">
<title id="chart-title">Returned utilisation against arc coordinate</title>
<path d="M90 45 V300 H760" class="axis" fill="none"/>
<path id="utilisation-limit" d="M90 {y(limit):.3f} H760" stroke="#d9820a" stroke-width="2" stroke-dasharray="6 4"/>
<text x="750" y="{y(limit)-10:.3f}" text-anchor="end">Screen limit: {limit:.1%}</text>
<text x="80" y="{y(0):.3f}" text-anchor="end">{0:.1%}</text>
<text x="80" y="{y(limit):.3f}" text-anchor="end">{limit:.1%}</text>
<polyline points="{points}" fill="none" stroke="#009b84" stroke-width="3"/>
{''.join(dots)}{ticks}
<text x="430" y="350" text-anchor="middle" class="axtitle">Arc coordinate [m]</text>
<text x="22" y="165" transform="rotate(-90 22 165)" text-anchor="middle" class="axtitle">Utilisation [%]</text>
</svg><p id="chart-detail" role="status">Select or hover a plotted section for demands and the comparison limit.</p>
<figcaption>Figure B. Calculated tension and bending screening; the dashed line is the implemented comparison threshold. Pressure values are echoed and are not included in utilisation. Keyboard focus and pointer hover expose the same values.</figcaption></figure>'''


INTERACTION = r'''
(() => {
  'use strict';
  const evidence = JSON.parse(document.getElementById('calculation-data').textContent);
  const byId = id => document.getElementById(id);
  document.querySelectorAll('.chart-point').forEach(point => {
    const show = () => { byId('chart-detail').textContent = point.getAttribute('aria-label'); };
    point.addEventListener('pointerenter', show);
    point.addEventListener('focus', show);
    point.addEventListener('click', show);
  });
  const state = {report: evidence.changes.report, revision: evidence.changes.revision,
    sources: evidence.sources, comments: []};
  const message = text => { byId('comment-status').textContent = text; };
  const show = () => { byId('comment-preview').textContent = JSON.stringify(state.comments, null, 2); };
  byId('comments-form').addEventListener('submit', event => {
    event.preventDefault();
    const text = byId('comment-text').value.trim();
    if (!text) return message('Enter a comment.');
    state.comments.push({section: byId('comment-section').value, text});
    byId('comment-text').value = '';
    show(); message('Comment retained in this page. Save beside the HTML to retain it after closing.');
  });
  byId('save-comments').addEventListener('click', async () => {
    try {
      const data = JSON.stringify(state, null, 2) + '\n';
      if (typeof window.showDirectoryPicker === 'function') {
        const directory = await window.showDirectoryPicker({mode: 'readwrite'});
        const report = await directory.getFileHandle('index.html');
        const selected = await (await report.getFile()).text();
        if (selected !== '<!DOCTYPE html>\n' + document.documentElement.outerHTML + '\n') {
          // DOM interaction changes markup; compare the immutable calculation record.
          const parsed = new DOMParser().parseFromString(selected, 'text/html');
          if (parsed.getElementById('calculation-data')?.textContent !== byId('calculation-data').textContent)
            throw new Error('Choose the folder containing the matching analysis HTML.');
        }
        try {
          const prior = await directory.getFileHandle('index.comments.json');
          const previous = await (await prior.getFile()).text();
          if (previous !== data && !window.confirm('Replace the existing saved comments? Load that file first to retain earlier comments.')) return;
        } catch (error) { if (error.name !== 'NotFoundError') throw error; }
        const handle = await directory.getFileHandle('index.comments.json', {create: true});
        const writable = await handle.createWritable();
        await writable.write(data); await writable.close();
        if (await (await handle.getFile()).text() !== data) throw new Error('Saved comment readback differs.');
        message('Comments saved beside the analysis HTML and read back successfully.');
      } else {
        const url = URL.createObjectURL(new Blob([data], {type: 'application/json'}));
        const link = document.createElement('a'); link.href = url; link.download = 'index.comments.json'; link.click();
        setTimeout(() => URL.revokeObjectURL(url), 1000);
        message('Download requested. Move index.comments.json beside the HTML; load it to check the saved copy.');
      }
    } catch (error) { message(error.message); }
  });
  byId('load-comments').addEventListener('change', async event => {
    try {
      const file = event.target.files[0]; if (!file) return;
      const incoming = JSON.parse(await file.text());
      if (incoming.report !== state.report || incoming.revision !== state.revision ||
          JSON.stringify(incoming.sources) !== JSON.stringify(state.sources) || !Array.isArray(incoming.comments))
        throw new Error('Comment file does not match this analysis and source basis.');
      const sections = new Set([...byId('comment-section').options].map(option => option.value));
      if (incoming.comments.some(row => !row || !sections.has(row.section) || typeof row.text !== 'string'))
        throw new Error('Comment file contains an invalid section or comment.');
      state.comments = [...new Map([...state.comments, ...incoming.comments].map(row => [JSON.stringify(row), row])).values()];
      show(); message('Matching comments loaded; current comments retained.');
    } catch (error) { message(error.message); }
    event.target.value = '';
  });
})();
'''


def render(data: dict[str, Any]) -> str:
    """Write the complete engineering narrative from a single calculation basis."""
    n = lambda key, fmt=".1f", scale=1: number(data, key, fmt, scale)
    result = data["result"]
    governing = result["results"].index(result["governing_result"])
    worst = f"result.results.{governing}."
    limit = f'{data["limit"]:.1%}'
    summary = f'''<p>The code checks module is an offline calculator for supplied engineering loads. Analysis is performed to determine the tension and bending screening response of a published riser example; key results are summarized below.</p>
<ul><li>The calculated maximum utilisation is {n('result.governing_utilisation', '.1%')} at {n(worst+'arc_length')} m, below the implemented screen limit of {limit}.<ul><li>The supplied load distribution can be ranked by section using this calculation. The example inputs are ASSUMED - to be confirmed.</li></ul></li>
<li>Pressure interaction and project design acceptance are NOT_EVALUATED.<ul><li>Release for design use is conditional on an applicable equation source, checked factors, pressure interaction and independent benchmark evidence.</li></ul></li></ul>
<p>The calculation and its restrictions are set out in <a href="#worked-example">the worked example</a> and <a href="#limits">the limits of use</a>.</p>
<div id="revision-history"><h3>Revision history</h3>'''
    summary += table(["Revision", "Date", "What changed and why"], REVISION_HISTORY, "Revision record. Each issue retains the preceding history.") + "</div>"
    introduction = '''<p>The module is used to compare caller-supplied section loads with implemented resistance expressions. No global structural response, sea-state limit or installation operating envelope is calculated.</p>
<p>The principal workflow is the offline riser router. A configuration mapping is read, pipe properties and station loads are passed to the load checker, and station results with a governing summary and comma-separated output are returned. The documented <a href="#source-example">public example</a> and <a href="#source-workflow">workflow registry</a> provide the calculation basis.</p>
<p>The calculation is an engineering-calculations report. Marine installation controls require a coupled analysis and are outside this scalar load screen.</p>'''
    basis = f'''<p>The analysis basis is the public repository example and the equation actually called by its router. The comparison threshold of {limit} is taken from the <a href="#source-checker">implemented Boolean comparison</a>; it is not an independently established design acceptance criterion.</p>
<p>Geometry, material strengths, load values, temperature multiplier and the design factor are demonstration inputs: ASSUMED - to be confirmed. The named riser standard and edition are recorded in <a href="#references">the references</a>. An equation-specific clause, applicability assessment and project factor selection are not established by the repository example; code qualification is NOT_EVALUATED.</p>
<p>The rationale for the displayed limit is a dimensionless demand-to-resistance comparison. The returned pass flag follows the checker after its utilisation rounding. Results close to the threshold require an unrounded independent assessment before a design decision.</p>''' + schematic(data)
    theory = '''<p>The section response is calculated from an annular steel area and an annular plastic bending modulus. The implemented utilisation is an elliptical tension and bending interaction; pressure is not a term in this equation.</p>
<div class="eq"><span>d = D − 2t; A = π(D² − d²)/4; Z = (D³ − d³)/6</span></div>
<div class="eq"><span>Tᵧ = fᵧ η A; Mₚ = fᵧ η Z; U = √[(T/Tᵧ)² + (M/Mₚ)²] / f</span></div>
<p>D and d are the outside and inside diameters, t is wall thickness, A is steel area, Z is plastic section modulus, fᵧ is specified minimum yield stress, η is the temperature multiplier, Tᵧ is yield tension, Mₚ is plastic bending resistance, T is effective tension, M is bending moment, f is the supplied design factor and U is utilisation. Geometry and stress use compatible length and force units; the checker converts resistance to the force and moment units of the supplied loads.</p>
<p>The numerical divisors and powers in the section equations are geometric definitions, not tabulated design factors. The equation transcription and unit conversions are evidenced by the <a href="#source-checker">called checker</a> and checked by <a href="#source-report-test">the rebuild test</a>. The separate plastic-moment property on the input model is not used by this checker and is not substituted here.</p>'''
    io = '''<p>The input mapping is supplied with geometry, material properties, factors and equal-length arc-coordinate and load arrays. A single load value can be broadcast by the router; the example supplies explicit arrays.</p>
<p>Outside diameter and wall thickness use metres, yield and ultimate strengths use pascals, effective tension uses kilonewtons, bending moment uses kilonewton metres, pressure difference uses megapascals and utilisation is dimensionless. The complete example input register is given in <a href="#data-register">the appendix</a>.</p>
<p>The output contains station coordinates, tension, bending moment, an echoed pressure difference, utilisation and a Boolean screen result. The summary contains the governing station, maximum utilisation, count of stations and overall screen flag. The comma-separated output holds the station rows. A calculation-source citation sidecar is not returned by this workflow.</p>
<p>The supplied ultimate strength is retained by the input model but is not used in this interaction equation. Pressure is echoed without affecting utilisation. Load signs are removed by squaring in the interaction; cyclic fatigue and load history are not assessed.</p>'''
    rows = []
    for index, row in enumerate(result["results"]):
        key = f"result.results.{index}."
        rows.append([n(key+"arc_length"), n(key+"tension_kN"), n(key+"bending_moment_kNm"), n(key+"pressure_diff_MPa"), n(key+"utilisation", ".1%"), "Screen passes." if row["pass"] else "Screen exceeds limit."])
    example = f'''<p>The public example is evaluated by calling the documented router with its published input mapping. The values below are returned by that call; scenario inputs remain ASSUMED - to be confirmed.</p>
<p>The maximum utilisation is {n('result.governing_utilisation', '.1%')} at {n(worst+'arc_length')} m. The comparison is with the implemented screen threshold described in <a href="#basis">the analysis basis</a>. Pressure interaction remains NOT_EVALUATED.</p>'''
    example += table(["Arc coordinate [m]", "Tension [kN]", "Bending moment [kN m]", "Pressure echo [MPa]", "Utilisation [%]", "Screen result"], rows, 'Station results. Source: <a href="#source-example">public example</a> evaluated by the <a href="#source-router">offline router</a>.', "station-results")
    example += chart(data)
    count = sum(item["definitions"] for item in data["test_inventory"])
    validation = f'''<p>The repository evidence is assessed as regression and arithmetic evidence. The inspected inventory contains {count} test function definitions across the calculation, router and example suites; this is a source inventory, not a count of independently validated engineering cases.</p>
<p>The existing tests check low and high loads, increased utilisation with increased demand, factor sensitivity, governing-summary output, unknown selector rejection, pipeline collapse and propagation arithmetic, and tank-shell sizing. Published examples exist for riser loads, mooring loads and tank-shell sizing. The inventory and source locators are given in <a href="#qa">the appendix</a>.</p>
<p>This report adds direct recalculation of every displayed example value, a separate evaluation of the implemented annular interaction equation, and a pressure-variation check demonstrating that pressure does not enter the result. The rebuild also checks source fingerprints and exact delivered bytes.</p>
<p>No independent published riser benchmark, measurement record or edition-specific compliance assessment is established by these tests. The worked example is a repository demonstration rather than an external benchmark.</p>'''
    limits = '''<p>The results are limited to the implemented tension and bending screen for the supplied section loads. Use as a complete pressure-containing riser assessment is not established.</p>
<h3>Limits of use</h3><p>Pressure interaction, collapse, local buckling, fatigue, corrosion loss, installation dynamics and global stability are not evaluated in the selected workflow. A separate standalone pipeline function does not extend this router's coverage. The router checks array presence and length, but a complete finite-value, geometry and load-basis preflight is not provided. The caller must establish positive inner diameter, finite inputs, consistent units and factored load combinations.</p>
<p>Utilisation is rounded before its Boolean comparison; a marginal screen result does not establish an unrounded margin. The example design factor and temperature multiplier do not establish project-specific applicability.</p>
<h3>Not covered</h3><p>Other available sub-areas are dynamic-riser screening using a separate offshore standard, mooring breaking-load screening, standalone pipeline containment and buckling resistance, and bottom-up atmospheric tank-shell sizing. These are identified as available interfaces; their equations and engineering qualification are not assessed in this analysis.</p>
<h3>Recommendations</h3><p>The next engineering step is to establish the applicable standard edition, equation clause, pressure interaction, material allowables, effective wall and load combinations before adopting a design limit. The named standards and the implemented exclusions are recorded in <a href="#references">the references</a>. Design release is conditional on an independent calculation or published benchmark with the same geometry, units and factors.</p>
<p>A first improvement slice is recommended: input validation for finite values and coupled geometry, followed by an explicitly sourced combined-loading assessment with pressure and an unrounded comparison. The <a href="#source-router">router</a> and <a href="#source-checker">checker</a> define that boundary. Until those checks are established, the current result is suitable only for reproducing and ranking this demonstration's tension and bending loads.</p>'''
    inputs = data["inputs"]
    fields = [("Outside diameter", "outer_diameter", "m", ".4f"), ("Wall thickness", "wall_thickness", "mm", ".3f"),
              ("Specified minimum yield stress", "smys", "Pa", ".1f"), ("Specified minimum ultimate stress", "smts", "Pa", ".1f"),
              ("Design factor", "design_factor", "dimensionless", ".2f"), ("Temperature multiplier", "temperature_derating", "dimensionless", ".1f")]
    register = [[label, n("inputs."+key, fmt, 1000 if key == "wall_thickness" else 1), unit, "ASSUMED - to be confirmed", '<a href="#source-example">Public example</a>'] for label, key, unit, fmt in fields]
    for label, key, unit in [("Arc coordinates", "arc_lengths", "m"), ("Effective tensions", "effective_tension_kN", "kN"), ("Bending moments", "bending_moment_kNm", "kN m"), ("Pressure differences", "pressure_diff_MPa", "MPa")]:
        register.append([label, ", ".join(n(f"inputs.{key}.{i}") for i in range(len(inputs[key]))), unit, "ASSUMED - to be confirmed", '<a href="#source-example">Public example</a>'])
    appendix = '''<p>The calculation basis, evidence locators and quality checks are retained below. Missing qualifications remain explicit and do not become accepted design inputs.</p>
<h3 id="data-register">9.1 Data register</h3>'''
    appendix += table(["Quantity", "Value", "Units", "Qualification", "Source"], register, "Input register. Values are read from the public example without substitution. Wall thickness is displayed in millimetres using the exact SI definition: one metre equals one thousand millimetres.")
    appendix += '''<p>Missing design data are the qualified equation clause, material and factor applicability, effective-wall basis, pressure interaction and independent benchmark. Each remains NOT_EVALUATED; the analysis makes no replacement engineering assumption for these items.</p>
<h3 id="references">9.2 References and locators</h3>
<p>The standard cited by the riser implementation is API STD 2RD, Dynamic Risers for Floating Production Systems, second edition (2013). Its historical repository selector uses RP naming. The implementation does not record a specific equation clause; that locator is NOT_EVALUATED. No licensed table or standard coefficient is reproduced as an established criterion.</p>
<p>Other module references are DNV-OS-F101, Submarine Pipeline Systems (2013), Section 5, Local buckling; DNV-OS-F201, Dynamic Risers (2010), Section 5, Combined loading; API RP 2SK, Design and Analysis of Stationkeeping Systems (2005), Section 6.2, Table 1; and API 650, Welded Tanks for Oil Storage, clause 5.6.3.2, Shell design and one-foot method (edition not established). The shell-method locator is recorded in the implementation; an independently qualified source is not established. These references are interface context only.</p>'''
    source_ids = {EXAMPLE: "source-example", CHECKER: "source-checker", "src/digitalmodel/code_checks/workflow.py": "source-router", "docs/registry/workflows.yaml": "source-workflow"}
    source_rows: list[list[str]] = []
    fingerprints = {source["path"]: source["sha256"] for source in data["sources"]}
    for path in (*SOURCE_PATHS, *REFERENCE_PATHS):
        source_rows.append([f'<span id="{source_ids.get(path, "ref-"+str(len(source_rows)))}">{escape(path)}</span>', fingerprints.get(path, "Context reference; outside the calculation fingerprint")])
    source_rows.append(['<span id="source-report-test">tests/code_checks/test_module_report.py</span>', "Displayed numeric evidence and independent equation comparison"])
    source_rows.append(["scripts/check_generated_html.py", "Registered freshness check for both delivered outputs"])
    appendix += table(["Repository source", "Source fingerprint or purpose"], source_rows, "Source register. Calculation fingerprints identify UTF-8 source text with line endings normalized to line feed; contextual references are listed without a digest.")
    appendix += '''<h3 id="qa">9.3 Quality evidence</h3><p>The source inventory below counts named test functions; parameterized cases are not expanded. The report rebuild checks numeric evidence, local section links, source fingerprints, embedded house style, comments controls and change history. Execution results are recorded with the delivered change description, not inferred from this inventory.</p>'''
    groups = ("Offline router", "Pipeline resistance", "Tank-shell sizing", "Published shell reports", "Riser and mooring arithmetic")
    inventory_rows = [[group, str(item["definitions"]), f'<a href="#ref-{len(SOURCE_PATHS) + REFERENCE_PATHS.index(item["path"])}">Source register</a>'] for group, item in zip(groups, data["test_inventory"])]
    appendix += table(["Evidence group", "Test definitions", "Source"], inventory_rows, "Regression evidence inventory. No field-validation or independent benchmark claim is attached to these counts.")
    appendix += '''<p>Preflight disposition: the public example and callable interface are present; source units and the implemented equation are inspected; numeric values are generated and linked to calculation evidence; pressure interaction and independent design qualification remain NOT_EVALUATED. No licensed solver is required.</p>'''
    comments = '''<p>Engineering changes are listed by revision so that the calculation basis can be compared with an earlier issue. Comments are attached to a named section and retained in a separate file beside the HTML.</p>
<div id="change-panel">'''
    comments += table(["Revision", "Engineering change", "Reason"], [[CHANGES["revision"], CHANGES["changes"][0]["description"], CHANGES["changes"][0]["reason"]]], "Changes since the preceding issue. The calculation equation and example inputs are unchanged.") + "</div>"
    sections = [("summary", "Executive Summary", summary), ("introduction", "Introduction", introduction),
                ("basis", "Analysis basis", basis), ("theory", "Theory and equations", theory),
                ("inputs-outputs", "Inputs and outputs", io), ("worked-example", "Worked example", example),
                ("validation", "Validation evidence", validation), ("limits", "Limits of use and recommendations", limits),
                ("appendices", "Appendices", appendix)]
    options = "".join(f'<option value="{identity}">{title}</option>' for identity, title, _ in sections)
    comments += f'''<form id="comments-form"><label>Section <select id="comment-section">{options}</select></label>
<p><label>Comment <textarea id="comment-text" rows="4" required></textarea></label></p>
<button type="submit">Add comment</button></form>
<p><button id="save-comments" type="button">Save comments beside HTML</button></p>
<p><label>Load saved comments <input id="load-comments" type="file" accept=".json,application/json"></label></p>
<p id="comment-status" role="status">Comments remain in this page until saved. Select the folder containing this HTML when prompted. Browsers without folder access download a file to be moved beside the HTML.</p><pre id="comment-preview">[]</pre>'''
    sections.append(("changes-comments", "Engineering changes and comments", comments))
    style = re.search(r"<style>.*?</style>", read(STYLE), re.S)
    if style is None:
        raise ValueError("The canonical house stylesheet is missing")
    css = style.group()
    nav = "".join(f'<li><a href="#{identity}">{title}</a></li>' for identity, title, _ in sections)
    evidence = json.dumps(data, ensure_ascii=False, sort_keys=True, allow_nan=False).replace("<", "\\u003c")
    return ('''<!DOCTYPE html>
<html lang="en" data-theme="light"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Code checks module analysis</title>''' + css + '''</head><body class="hs-doc doc">
<header class="masthead"><div class="wrap"><span class="brand">Engineering calculations</span><span>Draft for review</span></div></header>
<div class="hero"><div class="wrap"><h1>Code checks module analysis</h1><p class="lede">Offline riser tension and bending screening, calculation basis and limits of use.</p></div></div>
<div class="wrap"><nav aria-label="Contents"><ul>''' + nav + '</ul></nav><main class="main">'
            + "".join(section(*entry) for entry in sections)
            + '</main><footer><p>Draft for review. Design qualification is NOT_EVALUATED.</p></footer></div>\n'
            + f'<script type="application/json" id="calculation-data">{evidence}</script>\n'
            + f'<script id="interaction">{INTERACTION}</script>\n</body></html>\n')


def generate(output: Path) -> list[Path]:
    """Write deterministic UTF-8 outputs and verify saved bytes immediately."""
    output = Path(output)
    output.mkdir(parents=True, exist_ok=True)
    data = calculation()
    contents = {"index.html": render(data), "changes.json": json.dumps(CHANGES, indent=2, ensure_ascii=False) + "\n"}
    paths = []
    for name, content in contents.items():
        path = output / name
        encoded = content.encode("utf-8")
        path.write_bytes(encoded)
        if path.read_bytes() != encoded:
            raise OSError("Saved report readback differs")
        paths.append(path)
    return paths


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, required=True)
    generate(parser.parse_args().output)
