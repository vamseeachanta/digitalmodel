# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Rebuild the mooring fatigue report using the documented public inputs.

Use PYTHONPATH=src and run this script with --work-dir pointing to temporary
storage. Only report.html and changes.json are written to --output-dir.
The existing workflow writes its calculation CSVs to --work-dir.
"""

from __future__ import annotations

import argparse
import copy
import csv
import hashlib
import html
import json
import re
from pathlib import Path

import yaml

from digitalmodel.mooring_fatigue.workflow import router

ROOT = Path(__file__).resolve().parents[2]
EXAMPLE = ROOT / "examples/workflows/mooring-fatigue/input.yml"
EXEMPLAR = ROOT / "docs/reports/pipeline-ffs-assessment.html"
DESTINATION = ROOT / "docs/reports/modules/mooringfatigue"
TITLE = "Mooring fatigue module analysis"
SECTIONS = (
    "Executive Summary", "Introduction", "Design Data", "Analysis Methodology",
    "Detailed Results", "Key Findings", "Limitations", "Recommendations",
    "References", "Acronyms and Symbols",
)
CHANGES = {
    "report": "Mooring fatigue module analysis",
    "revision": "A",
    "date": "2026-10-22",
    "changes": [
        "The public metallic-line example is evaluated against the selected stress–life curve.",
        "The governing damage is compared with the design fatigue factor limit.",
        "The load-duration and component-curve gaps are identified before engineering acceptance.",
    ],
}

UI = r"""
(() => {
  'use strict';
  const el = id => document.getElementById(id);
  const changes = JSON.parse(el('change-data').textContent);
  el('change-record').textContent = JSON.stringify(changes, null, 2);
  for (const marker of document.querySelectorAll('[data-detail]')) {
    const show = () => { el('chart-detail').textContent = marker.dataset.detail; };
    marker.addEventListener('pointerenter', show);
    marker.addEventListener('focus', show);
  }
  const basis = el('comment-panel').dataset.basis;
  let comments = [];
  const message = text => { el('comment-status').textContent = text; };
  const preview = () => { el('comment-preview').textContent = JSON.stringify(comments, null, 2); };
  el('capture-quote').onclick = () => {
    el('comment-quote').value = String(window.getSelection()).slice(0, 10000);
    message('Selected text captured. Select its section before adding the comment.');
  };
  el('add-comment').onclick = () => {
    const text = el('comment-text').value.trim();
    if (!text) return message('Enter a comment.');
    comments.push({id: crypto.randomUUID(), section: el('comment-section').value,
      quote: el('comment-quote').value, text, date: new Date().toISOString()});
    el('comment-text').value = '';
    preview();
    message('Comment added. Save comments before closing this page.');
  };
  el('load-comments').onchange = async () => {
    try {
      const file = el('load-comments').files[0];
      if (!file) return;
      if (file.size > 1000000) throw new Error('The comment file exceeds the size limit.');
      const incoming = JSON.parse(await file.text());
      const sections = new Set([...el('comment-section').options].map(option => option.value));
      if (incoming.report !== changes.report || incoming.revision !== changes.revision ||
          incoming.basis !== basis || !Array.isArray(incoming.comments) ||
          incoming.comments.some(c => !c || typeof c.id !== 'string' ||
            typeof c.text !== 'string' || typeof c.quote !== 'string' ||
            typeof c.date !== 'string' || !sections.has(c.section)))
        throw new Error('Comments do not match this report and calculation basis.');
      const merged = new Map(comments.map(c => [c.id, c]));
      for (const comment of incoming.comments) {
        if (merged.has(comment.id) && JSON.stringify(merged.get(comment.id)) !== JSON.stringify(comment))
          throw new Error('A comment identifier has conflicting text; retain both files for reconciliation.');
        merged.set(comment.id, comment);
      }
      comments = [...merged.values()];
      preview();
      message('Matching comments loaded. Save comments to retain additions.');
    } catch (error) { message(error.message); }
    el('load-comments').value = '';
  };
  el('save-comments').onclick = async () => {
    const data = JSON.stringify({report: changes.report, revision: changes.revision,
      basis, comments}, null, 2) + '\n';
    try {
      if (typeof window.showSaveFilePicker === 'function') {
        const handle = await window.showSaveFilePicker({suggestedName: 'report.comments.json',
          types: [{description: 'Comments', accept: {'application/json': ['.json']}}]});
        const writable = await handle.createWritable();
        await writable.write(data);
        await writable.close();
        if (await (await handle.getFile()).text() !== data) throw new Error('Saved comments differ.');
        message('Comments saved and read back. Keep the comment file beside the report.');
      } else {
        const url = URL.createObjectURL(new Blob([data], {type: 'application/json'}));
        const link = document.createElement('a');
        link.href = url;
        link.download = 'report.comments.json';
        link.click();
        setTimeout(() => URL.revokeObjectURL(url), 1000);
        message('Download requested. Move report.comments.json beside the report and reload it to check the saved copy.');
      }
    } catch (error) { message(error.message); }
  };
})();
"""


def observations(work_dir: Path) -> tuple[dict, dict, list, dict]:
    """Call the public workflow and collect its returned, unrounded observations."""
    cfg = yaml.safe_load(EXAMPLE.read_text(encoding="utf-8"))
    inputs = copy.deepcopy(cfg["mooring_fatigue"])
    cfg["mooring_fatigue"]["output_dir"] = str(work_dir.resolve())
    result = router(cfg)["mooring_fatigue"]
    with Path(result["results_csv"]).open(newline="", encoding="utf-8") as stream:
        bins = list(csv.DictReader(stream))
    facts = {
        "design_life": result["design_life_years"], "dff": result["dff"],
        "damage_limit": 1 / result["dff"], "line_count": len(result["lines"]),
        "bin_count": len(bins), "governing_damage": result["governing_damage"],
        "governing_life": result["governing_fatigue_life_years"],
        "governing_margin": result["governing_dff_margin"],
    }
    for i, line in enumerate(result["lines"]):
        for key in ("total_damage", "fatigue_life_years", "required_life_years", "dff_margin"):
            facts[f"line.{i}.{key}"] = line[key]
        facts[f"line.{i}.area"] = inputs["lines"][i]["area_mm2"]
    for i, row in enumerate(bins):
        for key in ("tension_range_kN", "stress_range_MPa", "cycles", "allowable_cycles", "damage"):
            facts[f"bin.{i}.{key}"] = float(row[key])
    return inputs, result, bins, facts


def table(headers: tuple, rows: list, caption: str) -> str:
    """Render a table whose cells are already escaped or constructed markup."""
    return ('<div class="table-wrap"><table><caption>' + html.escape(caption)
            + '</caption><thead><tr>'
            + ''.join(f'<th scope="col">{html.escape(item)}</th>' for item in headers)
            + '</tr></thead><tbody>'
            + ''.join('<tr>' + ''.join(f'<td>{cell}</td>' for cell in row) + '</tr>' for row in rows)
            + '</tbody></table></div>')


def build(work_dir: Path) -> tuple[str, dict]:
    """Write the complete technical narrative around fresh module results."""
    inputs, result, bins, facts = observations(work_dir)

    def n(key: str, precision: str = ".1f") -> str:
        """Format an observation with a testable link to its returned value."""
        return (f'<span data-value="{key}" data-format="{precision}">'
                f'{format(facts[key], precision)}</span>')

    def source(number: int) -> str:
        """Link a claim to its reference record."""
        return f'<a href="#source-{number}">[Reference {number}]</a>'

    style_match = re.search(r"<style>.*?</style>", EXEMPLAR.read_text(encoding="utf-8"), re.S)
    if style_match is None:
        raise ValueError("The report exemplar has no stylesheet")
    style = style_match.group()
    line_rows = []
    area_rows = []
    markers = []
    limit = facts["damage_limit"]
    max_damage = max(limit, *(line["total_damage"] for line in result["lines"])) * 1.15

    def y(value: float) -> str:
        """Project damage onto the chart ordinate without changing its value."""
        return f"{270 - 220 * value / max_damage:.1f}"

    limit_y = y(limit)
    for i, line in enumerate(result["lines"]):
        label = html.escape(line["line_id"])
        line_rows.append((label, n(f"line.{i}.total_damage", ".4f"),
                          n(f"line.{i}.fatigue_life_years"),
                          n(f"line.{i}.required_life_years"), n(f"line.{i}.dff_margin"),
                          "The screening criterion is met." if line["passes_dff"]
                          else "The screening criterion is not met.", source(2)))
        area_rows.append((label, html.escape(line["material"]), n(f"line.{i}.area"), source(1)))
        x = 220 + i * 300
        detail = (f"Public example; component {line['line_id']}; demand {line['total_damage']:.4f}; "
                  f"limit {limit:.4f}; units: dimensionless damage.")
        markers.append(f'<g tabindex="0" role="img" aria-label="{html.escape(detail)}" '
                       f'data-detail="{html.escape(detail)}">'
                       f'<circle cx="{x}" cy="{y(line["total_damage"])}" r="9" fill="var(--report-accent)"/>'
                       f'<text x="{x}" y="300" text-anchor="middle">{label}</text>'
                       f'<title>{html.escape(detail)}</title></g>')
    chart = f"""<figure id="damage-chart">
<svg viewBox="0 0 800 340" role="img" aria-labelledby="damage-title damage-desc">
<title id="damage-title">Metallic-line example: damage against the screening limit</title>
<desc id="damage-desc">Damage is shown by component with the design fatigue factor limit drawn. Focus or hover over each marker for the demand and limit.</desc>
<path d="M90 40 V270 H740" fill="none" stroke="var(--ink)"/>
<text x="20" y="35">Damage (dimensionless)</text>
<text x="90" y="290">0</text>
<line x1="90" x2="740" y1="{limit_y}" y2="{limit_y}" stroke="var(--ink)" stroke-dasharray="7 5" data-limit="damage_limit"/>
<text x="105" y="{float(limit_y) - 10:.1f}">Screening limit</text>
{''.join(markers)}<text x="400" y="330" text-anchor="middle">Component</text>
</svg>
<p id="chart-detail" aria-live="polite">Focus or hover over a component marker to read its demand and limit.</p>
<figcaption>Figure A. The screening criterion is met when line damage does not exceed {n('damage_limit', '.4f')}.
The criterion is derived from the example design fatigue factor of {n('dff')}; it is not a component qualification. {source(1)} {source(2)}</figcaption>
</figure>"""
    bin_rows = []
    for i, row in enumerate(bins):
        bin_rows.append((html.escape(row["line_id"]), n(f"bin.{i}.tension_range_kN"),
                         n(f"bin.{i}.stress_range_MPa"), n(f"bin.{i}.cycles", ".0f"),
                         n(f"bin.{i}.allowable_cycles", ".0f"), n(f"bin.{i}.damage", ".4f"),
                         source(1) + " " + source(2)))
    change_items = ''.join(f'<li>{html.escape(item)}</li>' for item in CHANGES["changes"])
    revision = table(("Revision", "Date", "What changed and why"), [
        ("A", CHANGES["date"], "The metallic-line workflow is documented to distinguish a reproducible screening calculation from a component assessment.")
    ], "Revision history")
    sections = [f"""
<p>The mooring fatigue module is a metallic-line fatigue screening tool. Analysis is performed to determine cumulative fatigue damage under the documented public loading example, and the key results are summarized below.</p>
<ul><li>The example is retained as a calculation demonstration.
<ul><li>{html.escape(result['governing_line'])} governs at damage {n('governing_damage', '.4f')} against the {n('damage_limit', '.4f')} screening limit. {source(1)} {source(2)}</li>
<li>The governing fatigue life is {n('governing_life')} years against {n('line.1.required_life_years')} years required by the example design fatigue factor. These are extrapolations on assumed lifetime cycle counts. {source(1)} {source(2)}</li></ul></li>
<li>Engineering acceptance is deferred pending a component and exposure basis.
<ul><li>The selected generic curve is used for both metallic material labels; chain and wire qualification is NOT_EVALUATED. {source(2)}</li>
<li>Confirm the exposure duration and component stress–life basis before adopting an operating or inspection limit. {source(1)} {source(2)}</li></ul></li></ul>
{chart}
<div id="revision-history"><h3>Revision history</h3>{revision}</div>
""", f"""
<p>The metallic-line fatigue calculation is described for tension ranges supplied at a mooring component. A screening analysis is performed; platform motion, water depth and mooring equilibrium are not solved by this module. {source(2)}</p>
<p>The documented public example is used without changing its loads, areas, curve choice or design fatigue factor. It is a published software example rather than a measured loading record or an experimental benchmark. {source(1)}</p>
<p>The main workflow is tension-range bins to nominal stress ranges, cumulative damage, fatigue life and screening margin. Other input routes are listed under Not covered.</p>
""", f"""
<p>The public example inputs are retained as the calculation basis. Their suitability for a physical mooring system is not established. {source(1)}</p>
<h3>Site, host and component data</h3>
<p>Water depth, host response, mooring length, anchor position, pretension, local geometry and measured stress concentration are NOT_EVALUATED because they are absent from the example. Effective load-bearing areas are supplied directly; they are not inferred from a chain diameter or wire construction. {source(1)}</p>
{table(('Component', 'Material label', 'Effective area (mm²)', 'Source'), area_rows, 'Table A. Public example component inputs.')}
<p>The supplied design life is {n('design_life')} years and the design fatigue factor is {n('dff')}. The selected curve is category D in seawater with cathodic protection. These are example settings, not prescribed design criteria. {source(1)}</p>
<h3>Criteria and data status</h3>
<p>The screening criterion is defined by the implemented margin: predicted life divided by required life is at least unity. Required life is design life multiplied by the supplied design fatigue factor. Thus the example damage limit is {n('damage_limit', '.4f')}. This criterion is used because it is the module decision rule; no regulatory acceptance is inferred. {source(2)}</p>
<p>The curve record cites DNV-RP-C203, October 2011, Fatigue Design of Offshore Steel Structures, section 2.4, S–N curves, and Table 2-2, seawater with cathodic protection. The implementation also carries a later edition label whose numerical equivalence is not established by that record. No licensed coefficient is reproduced here. {source(3)}</p>
<p>ASSUMED - to be confirmed: the supplied cycle bins represent the entire design-life exposure. ASSUMED - to be confirmed: the supplied areas and selected curve form a consistent nominal stress basis. No missing physical dimension or material coefficient is assigned. The data register is provided in Appendix B.</p>
""", f"""
<p>The supplied tension ranges are converted to nominal axial stress ranges before the existing stress–life helper is called. Damage is accumulated line by line and the line with greatest damage is selected as governing. {source(2)}</p>
<figure id="system-sketch"><svg viewBox="0 0 900 290" role="img" aria-labelledby="system-title system-desc">
<title id="system-title">Mooring line and local calculation boundary</title>
<desc id="system-desc">An inclined line joins a floating support to an anchor on the seabed. A segment near the fairlead is isolated with axial tension at both ends. Its effective area and cyclic tension range are inputs. Dimensions and environmental loading are not supplied.</desc>
<path d="M40 78 Q95 68 150 78 T260 78 M40 230 H860" fill="none" stroke="var(--ink)"/>
<path d="M100 42 H240 L220 70 H120 Z" fill="none" stroke="var(--ink)"/>
<path d="M215 70 Q265 210 410 230 L430 230" fill="none" stroke="var(--report-accent)" stroke-width="4"/>
<path d="M395 220 L410 240 L425 220 M410 230 V250 M390 250 H430" fill="none" stroke="var(--ink)"/>
<text x="105" y="30">Floating support</text><text x="45" y="110">Water surface</text>
<text x="60" y="255">Seabed</text><text x="345" y="278">Anchor restraint</text>
<text x="260" y="120">Inclined mooring line</text>
<rect x="540" y="95" width="190" height="44" fill="none" stroke="var(--report-accent)"/>
<path d="M490 117 H540 M730 117 H790 M490 117 L504 109 M490 117 L504 125 M790 117 L776 109 M790 117 L776 125" fill="none" stroke="var(--ink)"/>
<text x="535" y="70">Local axial segment</text><text x="580" y="123">Effective area A</text>
<text x="485" y="168">Applied tension range ΔT at each end</text>
<text x="485" y="195">Stress range Δσ = ΔT / A</text>
<text x="485" y="225">No geometry or equilibrium solution</text>
</svg><figcaption>Figure B. The local calculation boundary is shown within a physical mooring arrangement. The drawing is schematic and is not to scale; geometry, mesh and global boundary conditions are not supplied. No spatial mesh is used in the fatigue accumulation. {source(2)}</figcaption></figure>
<h3>Stress–life and cumulative damage equations</h3>
<p>Nominal stress range is calculated as Δσ = ΔT / A with force and area expressed in consistent units. A tension in kilonewtons is converted to newtons before division by area in square millimetres; the resulting stress is in megapascals. No additional stress concentration or thickness correction is applied in this route. {source(2)}</p>
<p>The helper represents the selected curve by N = Nₖ(Δσ / Sₖ)<sup>−m</sup>. Here N is allowable cycles, Nₖ is the knee cycle count, Sₖ is the knee stress range, and m is the applicable segment slope. The below-knee segment is anchored continuously at the knee. Both segments remain finite; an endurance cut-off is not applied. The stored curve source and the helper implementation are distinguished in the reference record. {source(3)} {source(4)}</p>
<p>Miner damage is calculated as D = Σ(nᵢ / Nᵢ), where nᵢ is applied cycles and Nᵢ is allowable cycles for bin i. Fatigue life is calculated as L = T / D, required life as Lᵣ = T × F, and screening margin as M = L / Lᵣ. T is the supplied design life and F is the supplied design fatigue factor. The decision M ≥ unity is equivalent to D ≤ the reciprocal of F. These are the implemented equations, conditional on the assumed lifetime exposure. {source(2)}</p>
<h3>Inputs, execution and outputs</h3>
<p>The documented command-line entry calls the engine with an input configuration. The same mooring workflow is called directly for the example calculation in this report. Required settings are positive design life, positive design fatigue factor, and a non-empty list of metallic lines with non-empty cycle bins. For tension bins, effective area is required. Curve and environment are selected explicitly here. {source(1)} {source(2)}</p>
<p>The returned configuration contains the governing line, damage, life, margin, screening status and line summaries. Separate comma-separated-value files contain per-bin tension range, stress range, applied cycles, allowable cycles and damage, and per-line total damage, predicted life, required life, margin and screening decision. The caller configuration is modified in place. {source(2)}</p>
<p>Reproduction instructions and exact interface locators are retained in section 9.2. Engineering inputs and results are kept separate from the comment file.</p>
""", f"""
<p>The documented example is calculated by calling the module with its original input data. The {n('bin_count', '.0f')} tension bins across {n('line_count', '.0f')} metallic lines are processed; no result value is inserted independently of the returned calculation. {source(1)} {source(2)}</p>
<h3>Damage and life</h3>
<p>{html.escape(result['governing_line'])} governs at damage {n('governing_damage', '.4f')} against the {n('damage_limit', '.4f')} screening limit. The governing fatigue life is {n('governing_life')} years against {n('line.1.required_life_years')} years required by the example design fatigue factor. These are extrapolations on assumed lifetime cycle counts. {source(1)} {source(2)}</p>
{table(('Component', 'Damage (dimensionless)', 'Predicted life (years)', 'Required life (years)', 'Margin (dimensionless)', 'Screening status', 'Source'), line_rows, 'Table B. Returned line summaries; this is a screening calculation, not a component qualification.')}
<p>The governing margin is {n('governing_margin')} against unity. The apparent life reserve depends on the same generic curve being suitable for the nominal stress input. No inspection interval or operational service life is established. The full bin calculation is provided in Appendix A. {source(2)}</p>
<h3>Existing validation evidence</h3>
<p>The existing registry test exercises the public mooring example through the engine. It checks the governing line, bin count, positive stresses and damages, monotonic allowable cycles, and the life and margin identities. The shared fatigue tests check damage accumulation, curve behaviour and cycle counting. These are software checks; physical chain and wire validation is NOT_EVALUATED. {source(5)}</p>
<p>The report rebuild check compares every displayed worked-example observation, including repeated summary values, with a fresh module call and returned per-bin file. Independent arithmetic checks the stress conversion, damage sum, life and margin equations. The evidence establishes reproducibility and arithmetic consistency, not component curve selection or measured fatigue performance. {source(6)}</p>
""", f"""
<p>The workflow is available for metallic-line screening and its example results are reproducible through the direct module interface. The greatest total damage controls the reported line because the same design life and design fatigue factor apply to every line. {source(2)}</p>
<p>The material labels control eligibility only: the same curve is passed to chain and wire. This is the governing qualification gap for physical use. The result is sensitive to effective area, exposure cycles, corrosion protection and curve applicability; none is established for a real installation by the example. {source(1)} {source(2)}</p>
<p>The public example is suitable for checking input and output handling. It does not establish mooring resistance, a watch circle, an inspection date or a permissible environmental envelope.</p>
""", f"""
<p>The calculation is limited to the supplied stress basis and cycle exposure. Acceptance of a physical mooring is deferred until that basis is established. {source(1)} {source(2)}</p>
<p>Global equilibrium, pretension response, seabed contact, bending, chain interlink contact, out-of-plane bending, fretting, mean-stress correction, local stress concentration, corrosion wastage and thickness correction are not evaluated in this workflow. Neither synthetic rope damage nor residual strength is evaluated. Synthetic material labels are rejected. {source(2)}</p>
<p>Input checks reject missing or nonpositive required scalars and empty line or bin lists. Finite-value checks and rejection of simultaneous input representations are not comprehensive. When several representations are supplied, stress bins take precedence over tension bins, then stress history, then tension history. Supply one representation and independently check finite input values. {source(2)}</p>
<p>The exposure period is not inferred or annualised. Increasing design life while holding lifetime cycle bins fixed changes the reported life without changing damage; design life is not an automatic loading multiplier. Raw calculated life is therefore not a service-life recommendation. {source(2)}</p>
<h3>Not covered</h3>
<p>Stress-range bins, stress histories, tension histories, history repetition factors, histogram sensitivity, alternative environments, parametric atlases and synthetic-rope workflows are not demonstrated here. The history route uses rainflow counting followed by bin-centre histogramming; binning convergence and repeat-block boundary effects require separate checks before use. {source(2)} {source(4)}</p>
""", f"""
<p>Acceptance is to be based on component evidence and a documented exposure model. The public example remains a screening calculation until those records are supplied. {source(1)} {source(2)}</p>
<p>Obtain a component-specific stress–life or tension–life basis, effective area and local stress definition for each metallic component. Confirm the selected environment and corrosion protection, then assess the applicability of the cited fatigue method before evaluating service loads. The present material labels do not perform that selection. {source(2)} {source(3)}</p>
<p>Document the period represented by each cycle bin and scale the loading to the design exposure before calculation. Retain the original counts, scaling basis and effective area with the assessment. This action addresses the exposure assumption underlying L = T / D. {source(2)}</p>
<p>Suspend acceptance when calculated damage exceeds the reciprocal of the adopted design fatigue factor, or when the adopted curve, protection condition or exposure basis ceases to apply. Recalculate using measured or qualified loads and component data; reduce the permitted loading or replace the affected component before accepting a result that exceeds that limit. This trigger follows the implemented margin criterion and does not assign an unsupported calendar deadline. {source(2)}</p>
<p>Add an independent component benchmark with documented geometry, load range, exposure duration and permissible damage before adopting this workflow for an installation. Add explicit finite-input and mutually exclusive input checks, and qualify the history binning separately. No passing software check substitutes for these data. {source(2)} {source(5)}</p>
""", f"""
<p>The reference basis is separated into publications and software records. Publication identifiers establish the method source; implementation records establish what was actually calculated.</p>
<h3>9.1 Publications</h3>
<p id="source-3">Reference 3. DNV-RP-C203, Fatigue Design of Offshore Steel Structures, October 2011, section 2.4, S–N curves, and Table 2-2, seawater with cathodic protection. The stored-value record states that it was checked against this edition. The later implementation label does not establish numerical equivalence to that later edition. Licensed coefficients and limits are not reproduced.</p>
<p id="source-4">Reference 4. pyLife, WoehlerCurve API documentation, stable documentation accessed 22 October 2026. The calculation uses the installed pyLife helper with the range-based knee supplied by the module; closed-loop and residual-cycle handling are defined in the repository rainflow helper. <a href="https://pylife.readthedocs.io/en/stable/materiallaws/woehlercurve.html">Public stress–life API documentation</a>.</p>
<h3>9.2 Software records and reproduction</h3>
<p id="source-1">Reference 1. The public example input and documented command are held in <code>examples/workflows/mooring-fatigue/input.yml</code> and <code>examples/workflows/mooring-fatigue/README.md</code>. No measured or external benchmark provenance is supplied there.</p>
<p id="source-2">Reference 2. The calculation entry point is <code>digitalmodel.mooring_fatigue.workflow.router(cfg)</code> in <code>src/digitalmodel/mooring_fatigue/workflow.py</code>, also routed through <code>src/digitalmodel/engine.py</code>. The curve and Miner helpers are <code>src/digitalmodel/fatigue/sn_curves.py</code> and <code>src/digitalmodel/fatigue/damage.py</code>. The stored edition basis is <code>src/digitalmodel/fatigue/c203_sn_tables.py</code> and <code>src/digitalmodel/fatigue/c203_editions.py</code>. History handling is <code>src/digitalmodel/fatigue/rainflow.py</code>.</p>
<p id="source-5">Reference 5. Existing evidence is held in <code>tests/workflows/test_durable_workflows.py::test_workflow_registry[mooring-fatigue]</code>, <code>tests/fatigue/test_damage.py</code>, <code>tests/fatigue/test_sn_curves.py</code>, <code>tests/fatigue/test_rainflow.py</code> and <code>tests/fatigue/test_c203_sn_values.py</code>. These tests are distinct from physical benchmark data.</p>
<p id="source-6">Reference 6. Rebuild and observation checks are held in <code>tests/reporting/test_mooring_fatigue_module_report.py</code>. The generator is <code>scripts/reporting/build_mooring_fatigue_module_report.py</code>. The layout exemplar is <code>docs/reports/pipeline-ffs-assessment.html</code>; its stylesheet is inlined without alteration. The required body class is retained; an exemplar using that class was not present in the inspected source.</p>
<p>From the repository root, with the source tree on the Python import path, run <code>python scripts/reporting/build_mooring_fatigue_module_report.py --work-dir &lt;temporary calculation folder&gt;</code>. The module runtime dependencies, including NumPy, pandas, pyLife, PyYAML and Pydantic, are required. The two report deliverables are regenerated; calculation files remain in the explicitly supplied temporary folder.</p>
""", """
<p>The symbols used in the equations and tables are defined below. Stress and tension are ranges throughout; no amplitude conversion is applied.</p>
<p>A is effective load-bearing area. ΔT is cyclic tension range. Δσ is nominal stress range. N is allowable cycles. Nₖ and Sₖ are knee cycles and knee stress range. m is the active stress–life slope. The subscript identifies a cycle bin. nᵢ is the applied cycles in that bin. D is cumulative Miner damage. T is design life. F is design fatigue factor. L is extrapolated fatigue life. Lᵣ is required fatigue life. M is screening margin.</p>
<p>MPa means megapascal; kN means kilonewton; mm² means square millimetre. DFF means design fatigue factor. S–N means stress–life. CP means cathodic protection. CSV means comma-separated values. API means application programming interface. JSON means JavaScript Object Notation. NOT_EVALUATED means that an engineering result has not been established from the available data.</p>
"""]
    body = ''.join(f'<section id="section-{i}"><h2>{i}. {title}</h2>{content}</section>\n'
                   for i, (title, content) in enumerate(zip(SECTIONS, sections), 1))
    appendix = f"""<div id="appendices"><h3>Appendix A. Worked-example bin detail</h3>
<p>Input ranges and counts are shown with the module-returned stress conversion, allowable cycles and damage. Values are rounded for display only; comparisons are made before rounding.</p>
{table(('Component', 'Tension range (kN)', 'Stress range (MPa)', 'Applied cycles', 'Allowable cycles', 'Damage (dimensionless)', 'Source'), bin_rows, 'Table C. Full bin calculation from the documented example.')}
<h3>Appendix B. Data register</h3>
{table(('Data', 'Status and consequence', 'Required action', 'Source'), [
    ('Cycle exposure', 'ASSUMED - to be confirmed: bins cover the entire design life.', 'Document exposure and repeat scaling before acceptance.', source(1) + ' ' + source(2)),
    ('Area and curve pairing', 'ASSUMED - to be confirmed: the supplied area and curve define a consistent nominal stress basis.', 'Supply chain and wire component evidence.', source(1) + ' ' + source(2)),
    ('Protection condition', 'ASSUMED - to be confirmed: the selected protected environment applies throughout the exposure.', 'Supply a corrosion protection basis and monitoring trigger.', source(1)),
    ('Geometry and local stresses', 'NOT_EVALUATED; no missing dimension is assigned.', 'Supply dimensions, stress concentration and bending basis.', source(1) + ' ' + source(2)),
    ('Physical validation', 'NOT_EVALUATED; no measured chain or wire benchmark is supplied by the example.', 'Supply an independent component benchmark.', source(1) + ' ' + source(5)),
], 'Table D. Data held and data required.')}
<h3>Appendix C. Quality assurance pre-flight</h3>
<p>The calculation is rebuilt through the module rather than a parallel fatigue formula. All displayed worked-example observations are checked against a separate module call. Per-bin stress conversion and damage summation are checked independently, followed by the life and margin identities. Material rejection and a damage-limit exceedance are exercised by the report tests. These checks are arithmetic and software evidence only. {source(6)}</p>
<p>The document checks cover the ten-section frame, revision location, unique anchors, working internal references, unchanged exemplar stylesheet, absence of remote script dependencies and visible wording. Browser interaction and component fatigue performance require separate assessment.</p>
<p>The screening criterion is met when damage does not exceed its adopted limit. The screening criterion is not met when that limit is exceeded. NOT_EVALUATED is used when the engineering basis is absent. No engineering status is inferred from a successful rebuild.</p></div>
"""
    # Bind saved comments to all executable calculation sources and to the prose.
    basis_inputs = [EXAMPLE, Path(__file__), *sorted((ROOT / "src/digitalmodel/mooring_fatigue").glob("*.py")),
                    *sorted((ROOT / "src/digitalmodel/fatigue").glob("*.py"))]
    basis = hashlib.sha256(
        b''.join(path.read_bytes() for path in basis_inputs)
        + json.dumps(facts, sort_keys=True, allow_nan=False).encode("utf-8")
    ).hexdigest()
    options = ''.join(f'<option value="section-{i}">{title}</option>' for i, title in enumerate(SECTIONS, 1))
    options += '<option value="appendices">Appendices</option>'
    comments = f"""<div id="comment-panel" data-basis="{basis}"><h3>Document comments</h3>
<p>Comments are saved separately from the calculation. Select report text, capture the quote, choose the section and add a comment. Save the comment file beside this report. Where a save dialog is unavailable, move the downloaded file beside the report. Load that file when reopening; unsaved comments are lost when the page closes.</p>
<p><button id="capture-quote" type="button">Capture selected text</button>
<label>Section <select id="comment-section">{options}</select></label></p>
<p><label>Quoted text <textarea id="comment-quote" rows="3" cols="65"></textarea></label></p>
<p><label>Comment <textarea id="comment-text" rows="4" cols="65"></textarea></label></p>
<p><button id="add-comment" type="button">Add comment</button>
<button id="save-comments" type="button">Save comments beside report</button>
<label>Load matching comments <input id="load-comments" type="file" accept=".json"></label></p>
<p id="comment-status" aria-live="polite">No comments are saved automatically.</p>
<pre id="comment-preview" aria-label="Comment preview">[]</pre></div>"""
    nav = ''.join(f'<li><a href="#section-{i}">{title}</a></li>' for i, title in enumerate(SECTIONS, 1))
    page = f"""<!DOCTYPE html>
<html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">
<title>{TITLE}</title>
{style}
</head><body class="hs-doc">
<header><h1>{TITLE}</h1><p>Draft for client review</p><p>Revision A | {CHANGES['date']}</p></header>
<div class="layout"><nav aria-label="Contents"><ol>{nav}<li><a href="#appendices">Appendices</a></li>
<li><a href="#comment-panel">Document comments</a></li></ol></nav><main>
<details id="change-panel"><summary>Engineering changes since the previous revision</summary><ul>{change_items}</ul>
<details><summary>Change record</summary><pre id="change-record"></pre></details></details>
{body}{appendix}{comments}
</main></div>
<script type="application/json" id="change-data">{json.dumps(CHANGES, ensure_ascii=False)}</script>
<script>{UI}</script>
</body></html>
"""
    return page, copy.deepcopy(CHANGES)


def main() -> None:
    """Build only the report deliverables in the requested output location."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--work-dir", type=Path, required=True)
    parser.add_argument("--output-dir", type=Path, default=DESTINATION)
    args = parser.parse_args()
    page, changes = build(args.work_dir)
    args.output_dir.mkdir(parents=True, exist_ok=True)
    (args.output_dir / "report.html").write_text(page, encoding="utf-8")
    (args.output_dir / "changes.json").write_text(
        json.dumps(changes, indent=2, ensure_ascii=False) + "\n", encoding="utf-8"
    )


if __name__ == "__main__":
    main()
