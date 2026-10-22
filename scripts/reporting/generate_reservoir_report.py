# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Rebuild the standalone reservoir analysis from public inputs and live calls.

Run with PYTHONPATH=src python scripts/reporting/generate_reservoir_report.py.
No network request or external analysis program is used by this generator.
"""

from __future__ import annotations

import argparse
from dataclasses import asdict
import hashlib
from html import escape
from html.parser import HTMLParser
import json
import math
from pathlib import Path
import re
from typing import Any

from digitalmodel.reservoir.volumetrics import (
    BBL_PER_ACRE_FT,
    VolumetricInputs,
    incremental_recovery_stb,
    ooip_stb,
    recoverable_stb,
)

ROOT = Path(__file__).resolve().parents[2]
DESTINATION = Path("docs/reports/modules/reservoir")
BRIEF = Path("docs/collide_pe/briefs/pe-2026-05-21.html")
STYLE_SOURCE = Path("docs/reports/pipeline-ffs-assessment.html")
EVIDENCE = DESTINATION / "validation.json"
REVISION = "1"
DATE = "2026-10-22"
AAPG = "https://wiki.aapg.org/index.php?title=Reserves_estimation&oldid=27160"


class GivenTable(HTMLParser):
    """Extract the retained teaching brief's named inputs without copying prose."""

    def __init__(self) -> None:
        """Initialize table selection and the ordered input cells."""
        super().__init__()
        self.active = False
        self.in_cell = False
        self.cells: list[str] = []

    def handle_starttag(self, tag: str, attrs: list[tuple[str, str | None]]) -> None:
        """Select only cells in the documented Given table."""
        if tag == "table" and dict(attrs).get("class") == "given":
            self.active = True
        if tag == "td" and self.active:
            self.in_cell = True
            self.cells.append("")

    def handle_data(self, data: str) -> None:
        """Retain cell text for named numerical conversion."""
        if self.in_cell:
            self.cells[-1] += data

    def handle_endtag(self, tag: str) -> None:
        """Stop collecting when a cell or its table ends."""
        if tag == "td":
            self.in_cell = False
        if tag == "table":
            self.active = False


def public_inputs() -> tuple[VolumetricInputs, dict[str, float]]:
    """Use only the documented values in the repository's public brief."""
    table = GivenTable()
    table.feed((ROOT / BRIEF).read_text(encoding="utf-8"))
    fields = dict(zip(table.cells[::2], table.cells[1::2], strict=True))
    expected = {"area acres", "net pay ft", "porosity", "oil saturation",
                "formation volume factor boi", "primary recovery factor",
                "secondary recovery factor"}
    if set(fields) != expected:
        raise ValueError("Public teaching input table changed; inspect its basis")
    values = {key: float(value) for key, value in fields.items()}
    if not all(math.isfinite(value) for value in values.values()):
        raise ValueError("Teaching inputs must be finite")
    inputs = VolumetricInputs(values["area acres"], values["net pay ft"],
                             values["porosity"], values["oil saturation"],
                             values["formation volume factor boi"])
    factors = {"primary_factor": values["primary recovery factor"],
               "raised_factor": values["secondary recovery factor"]}
    if not (inputs.area_acres > 0 and inputs.net_pay_ft > 0
            and inputs.formation_volume_factor > 0
            and 0 <= inputs.porosity <= 1 and 0 <= inputs.oil_saturation <= 1
            and 0 <= factors["primary_factor"] <= factors["raised_factor"] <= 1):
        raise ValueError("Teaching inputs violate the stated physical domain")
    return inputs, factors


def digest(path: str | Path) -> str:
    """Identify UTF-8 source text after normalizing line endings to LF."""
    text = (ROOT / path).read_text(encoding="utf-8")
    return hashlib.sha256(text.encode("utf-8")).hexdigest()


def calculation() -> dict[str, Any]:
    """Compute every result and chart sample through the public module."""
    inputs, factors = public_inputs()
    oil = ooip_stb(inputs)
    results = {"oil_in_place": oil,
               "primary": recoverable_stb(oil, factors["primary_factor"]),
               "raised": recoverable_stb(oil, factors["raised_factor"]),
               "increment": incremental_recovery_stb(
                   oil, factors["primary_factor"], factors["raised_factor"])}
    values = {**asdict(inputs), **factors, "coefficient": BBL_PER_ACRE_FT, **results}
    # The closed physical fraction interval is sampled for display, not as a
    # reservoir performance forecast or an estimated uncertainty distribution.
    curve = [{"factor": index / 100, "volume": recoverable_stb(oil, index / 100),
              "limit": oil} for index in range(101)]
    return {"inputs": asdict(inputs), "factors": factors, "results": results,
            "values": values, "curve": curve}


def evidence_record() -> dict[str, Any]:
    """Reject stale outcome records rather than presenting them as current."""
    evidence: dict[str, Any] = json.loads((ROOT / EVIDENCE).read_text(encoding="utf-8"))
    for path, expected in evidence["source_digests"].items():
        if digest(path) != expected:
            raise ValueError(f"Validation evidence is stale: {path}")
    observed = {key: 0 for key in ("passed", "skipped", "failed", "errors")}
    for case in evidence["cases"]:
        observed[case["outcome"]] += 1
    if observed != evidence["counts"] or observed["failed"] or observed["errors"]:
        raise ValueError("Validation evidence has inconsistent or failing outcomes")
    return evidence


def value(data: dict[str, Any], key: str, fmt: str = ",.1f", tag: str = "span") -> str:
    """Bind visible numerical text to a named raw module input or result."""
    number = data["values"][key]
    return (f'<{tag} data-value="{key}" data-raw="{number!r}" '
            f'data-format="{fmt}">{format(number, fmt)}</{tag}>')


def changes() -> dict[str, Any]:
    """Describe engineering content since the preceding reader revision."""
    return {"report": "reservoir-volumetric-analysis", "revision": REVISION,
            "date": DATE, "previous_revision": "0",
            "changes": [{"section": "section-9", "description":
                         "Source identities use LF-normalized text and validation covers reservoir sources and tests only."},
                        {"section": "section-10", "description":
                         "Comment files follow the report revision and imports preserve existing comments without exact duplicates."}]}


def recovery_chart(data: dict[str, Any]) -> str:
    """Draw the module samples with a physically defined upper-bound line."""
    oil = data["results"]["oil_in_place"]
    points = " ".join(f'{70 + p["factor"] * 700:.2f},{300 - p["volume"] / oil * 240:.2f}'
                      for p in data["curve"])
    return f'''<figure><svg id="recovery-chart" viewBox="0 0 840 380"
role="img" aria-labelledby="curve-title curve-description">
<title id="curve-title">Recoverable oil against recovery fraction</title>
<desc id="curve-description">Recovery demand increases with the selected fraction. The dashed line is oil in place, the physical upper bound, rather than an operating acceptance criterion.</desc>
<path d="M70 60V300H770" fill="none" stroke="currentColor"/>
<line id="physical-limit" x1="70" y1="60" x2="770" y2="60" stroke="var(--muted)" stroke-dasharray="7 5"/>
<text x="75" y="47">Physical upper bound = {value(data, 'oil_in_place', tag='tspan')} STB</text>
<polyline points="{points}" fill="none" stroke="var(--report-accent)" stroke-width="3"/>
<circle id="curve-marker" cx="70" cy="300" r="6" fill="var(--report-accent)"/>
<text x="60" y="322">0</text><text x="750" y="322">100</text>
<text x="330" y="354">Recovery fraction (%)</text>
<text transform="translate(23 255) rotate(-90)">Recoverable oil (STB)</text>
</svg><label for="recovery-selector">Select recovery fraction (%)</label>
<input id="recovery-selector" type="range" min="0" max="100" value="{round(data['factors']['primary_factor'] * 100)}" step="1">
<output id="chart-readout" for="recovery-selector" aria-live="polite"></output>
<figcaption>Figure 6.1. The documented teaching volume is held fixed. Hover over the curve or use the selector. The dashed bound corresponds to complete recovery; it is not a proposed recovery target. Source: the public volumetric functions and the recovery definition in the open reserves reference.</figcaption></figure>'''


def schematic(data: dict[str, Any]) -> str:
    """Identify the static geological control volume without invented dimensions."""
    return f'''<figure><svg viewBox="0 0 900 380" role="img" aria-labelledby="rock-title rock-desc">
<title id="rock-title">Conceptual sealed sandstone accumulation and modeled pay</title>
<desc id="rock-desc">A curved seal overlies a porous oil-bearing interval and underlying water. Net pay thickness and map area are inputs. No flow or pressure boundary is solved, and no measured geometry is represented.</desc>
<defs><pattern id="sand" width="12" height="12" patternUnits="userSpaceOnUse"><circle cx="3" cy="4" r="1" fill="var(--muted)"/></pattern></defs>
<path d="M40 180Q340 20 650 180L650 210Q340 65 40 210Z" fill="var(--line)" stroke="var(--muted)"/>
<path d="M40 210Q340 65 650 210L650 265H40Z" fill="url(#sand)" stroke="var(--muted)"/>
<path d="M130 210Q340 115 560 210Z" fill="var(--report-accent)" opacity=".45"/>
<path d="M40 265H650V310H40Z" fill="var(--line)"/>
<path d="M130 210H560" stroke="var(--ink)" stroke-dasharray="5 5"/>
<path d="M340 65V210" stroke="var(--ink)" stroke-width="5"/>
<path d="M350 151H470M350 210H470M455 151V210M450 157L455 151L460 157M450 204L455 210L460 204" fill="none" stroke="var(--ink)"/>
<text x="55" y="50">Seal above porous sandstone</text><path d="M155 60L180 110" stroke="var(--ink)"/>
<text x="358" y="80">Well intersection</text>
<text x="478" y="177">Net pay h</text><text x="478" y="199">{value(data, 'net_pay_ft', ',.3f', 'tspan')} ft</text>
<text x="165" y="245">Oil-bearing pores above water contact</text><text x="270" y="295">Underlying water</text>
<ellipse cx="767" cy="155" rx="90" ry="54" fill="url(#sand)" stroke="var(--ink)"/>
<text x="692" y="78">Map outline</text><text x="695" y="240">Area A</text>
<text x="690" y="263">{value(data, 'area_acres', tag='tspan')} acres</text>
<text x="55" y="350">Static volume; lateral shape and burial depth are not supplied.</text>
</svg><figcaption>Figure 3.1. Conceptual control volume. Porosity and oil saturation describe the pore space; the formation volume factor converts reservoir oil volume to stock-tank volume. Shape is schematic and is not a geological interpretation of measured data.</figcaption></figure>'''


INTERFACE = r'''
(function () {
  const data = JSON.parse(document.getElementById('calculation-data').textContent);
  const selector = document.getElementById('recovery-selector');
  const chart = document.getElementById('recovery-chart');
  const marker = document.getElementById('curve-marker');
  function show(index) {
    const point = data.curve[index];
    selector.value = index;
    marker.setAttribute('cx', 70 + point.factor * 700);
    marker.setAttribute('cy', 300 - point.volume / point.limit * 240);
    const fmt = number => number.toLocaleString('en-US', {minimumFractionDigits: 1, maximumFractionDigits: 1});
    const text = `Teaching example; oil volume; recovery fraction ${fmt(point.factor * 100)} %; demand ${fmt(point.volume)} STB; physical limit ${fmt(point.limit)} STB.`;
    document.getElementById('chart-readout').textContent = text;
    marker.replaceChildren();
    const title = document.createElementNS('http://www.w3.org/2000/svg', 'title');
    title.textContent = text; marker.append(title);
  }
  selector.addEventListener('input', () => show(Number(selector.value)));
  chart.addEventListener('pointermove', event => {
    const point = new DOMPoint(event.clientX, event.clientY).matrixTransform(chart.getScreenCTM().inverse());
    show(Math.max(0, Math.min(100, Math.round((point.x - 70) / 7))));
  });
  show(Math.round(data.factors.primary_factor * 100));

  const change = JSON.parse(document.getElementById('changes-data').textContent);
  const list = document.getElementById('change-list');
  change.changes.forEach(item => {
    const li = document.createElement('li'); li.textContent = item.description; list.append(li);
  });
  let comments = [];
  const status = document.getElementById('comment-status');
  const allowed = new Set(Array.from(document.querySelectorAll('main > section')).map(node => node.id));
  function render() {
    const list = document.getElementById('comment-list'); list.replaceChildren();
    comments.forEach(item => {
      const li = document.createElement('li');
      li.textContent = `${document.getElementById(item.section).querySelector('h2').textContent}: ${item.text}`;
      list.append(li);
    });
  }
  document.getElementById('comment-add').addEventListener('click', () => {
    const input = document.getElementById('comment-text');
    const text = input.value.trim();
    if (!text) { status.textContent = 'Enter a comment before adding it.'; return; }
    comments.push({section: document.getElementById('comment-section').value, text});
    input.value = ''; render(); status.textContent = 'Comment added in memory. Save comments before closing.';
  });
  document.getElementById('comment-export').addEventListener('click', async () => {
    const bytes = JSON.stringify({report: 'reservoir-volumetric-analysis', revision: change.revision, comments}, null, 2);
    const suggestedName = 'reservoir.comments.json';
    if (window.showSaveFilePicker) {
      try {
        const handle = await window.showSaveFilePicker({suggestedName, types: [{description: 'Comments', accept: {'application/json': ['.json']}}]});
        const writable = await handle.createWritable(); await writable.write(bytes); await writable.close();
        status.textContent = 'Comments saved. Keep the comments file beside this report.'; return;
      } catch (error) {
        if (error.name === 'AbortError') { status.textContent = 'Save cancelled; comments remain in memory.'; return; }
      }
    }
    const url = URL.createObjectURL(new Blob([bytes], {type: 'application/json'}));
    const anchor = document.createElement('a'); anchor.href = url; anchor.download = suggestedName; anchor.click();
    setTimeout(() => URL.revokeObjectURL(url), 1000);
    status.textContent = 'Comments downloaded. Move the comments file beside this report.';
  });
  document.getElementById('comment-import').addEventListener('change', async event => {
    try {
      const file = event.target.files[0]; if (!file) return;
      if (file.size > 1000000) throw new Error('Comments file exceeds the interface size limit.');
      const incoming = JSON.parse(await file.text());
      if (incoming.report !== 'reservoir-volumetric-analysis' || incoming.revision !== change.revision || !Array.isArray(incoming.comments)
          || !incoming.comments.every(item => allowed.has(item.section) && typeof item.text === 'string' && item.text.length <= 10000)) {
        throw new Error('Comments do not match this report and revision.');
      }
      incoming.comments.forEach(({section, text}) => {
        if (!comments.some(item => item.section === section && item.text === text)) {
          comments.push({section, text});
        }
      });
      render(); status.textContent = 'Saved comments merged. Save comments before closing.';
    } catch (error) { status.textContent = error.message; }
  });
})();
'''


def report_body(data: dict[str, Any]) -> str:
    """Write the engineering narrative with all package evidence in context."""
    def v(key: str, fmt: str = ",.1f") -> str:
        return value(data, key, fmt)

    counts = data["evidence"]["counts"]
    return f'''
<header><h1>Reservoir volumetric analysis</h1><p>Draft for review</p>
<p>Revision {REVISION} · {DATE}</p></header><div class="layout">
<nav aria-label="Contents"><ol>
<li><a href="#section-1">Executive Summary</a></li><li><a href="#section-2">Introduction</a></li>
<li><a href="#section-3">System and Workflow</a></li><li><a href="#section-4">Data and Basis</a></li>
<li><a href="#section-5">Detailed Results</a></li><li><a href="#section-6">Recovery Sensitivity</a></li>
<li><a href="#section-7">Validation Evidence</a></li><li><a href="#section-8">Limits and Recommendations</a></li>
<li><a href="#section-9">References</a></li><li><a href="#section-10">Appendices</a></li>
</ol><details><summary>Engineering changes</summary><ul id="change-list"></ul></details></nav>
<main>
<section id="section-1"><h2>1. Executive Summary</h2>
<p>The reservoir module is a static oil-volume calculator and well-log display package. Analysis is performed to determine oil initially in place and the change in recoverable volume for specified recovery fractions; key results are summarized below.</p>
<ul><li>The public teaching example supports calculation review.
<ul><li>Oil initially in place is {v('oil_in_place')} stock-tank barrels. This is the physical upper bound for recovery from the stated volume.</li>
<li>Recoverable volume is {v('primary')} stock-tank barrels at the stated primary recovery fraction and {v('raised')} stock-tank barrels at the raised fraction.</li>
<li>The increase is {v('increment')} stock-tank barrels; the recovery fractions are scenario inputs rather than demonstrated reservoir performance.</li></ul></li>
<li>Field use remains conditional on evidence.
<ul><li>Arithmetic regression evidence is available. Field accuracy, a commercial reserves classification and a production schedule are not established.</li>
<li>Confirm mapped pay, fluid data and a recovery basis before applying a recovery scenario to a development decision.</li></ul></li></ul>
<p>Source: the retained teaching example, current scalar calculation and open reserves reference; see <a href="#section-5">Detailed Results</a> and <a href="#section-9">References</a>.</p>
</section>
<div id="revision-history"><h3>Revision history</h3><table><thead><tr><th>Revision</th><th>Date</th><th>What changed and why</th></tr></thead>
<tbody><tr><td>0</td><td>2026-10-21</td><td>Initial issue explains the scalar workflow and separates arithmetic evidence from reservoir qualification.</td></tr>
<tr><td>{REVISION}</td><td>{DATE}</td><td>Source identities and output line endings are normalized for reproducible builds; validation scope and comment handling are corrected.</td></tr></tbody>
<caption>Revision history. Engineering issues.</caption></table></div>

<section id="section-2"><h2>2. Introduction</h2>
<p>The public scalar workflow is assessed for a reservoir engineer reviewing an early volumetric estimate. The retained teaching problem is used to demonstrate the callable interface with a documented numerical basis.</p>
<p>The original teaching page is unavailable during source checking. Its published repository brief and matching regression fixture are used as the retained example record; independent confirmation of the original statement is not established. The open reserves reference supports the equations rather than the example inputs.</p>
<p>The assessment covers a calculation, not an actual field. No measured reservoir inputs or licensed calculation programs are used.</p>
</section>

<section id="section-3"><h2>3. System and Workflow</h2>
<p>The original oil volume is calculated from mapped area, net pay, porosity, initial oil saturation and the initial oil formation volume factor. Recovery scenarios are applied to that stock-tank volume.</p>
<p>The public input record is named VolumetricInputs. The callable functions ooip_stb, recoverable_stb and incremental_recovery_stb return scalar stock-tank volumes. A stock-tank barrel is abbreviated STB in equations and chart units.</p>
{schematic(data)}
<p>Mapped area and net pay describe the bulk rock volume. Porosity identifies the pore fraction, oil saturation identifies the oil-filled pore fraction, and the formation volume factor relates reservoir barrels to stock-tank barrels. No pressure distribution, transmissibility, flow boundary or computational mesh is solved.</p>
<h3>3.1 Not covered</h3>
<p>The stratigraphic display interface creates gamma-ray, resistivity, density-neutron and facies tracks aligned on formation tops. Its available checks use synthetic data and mocked plotting objects. Geological correlation accuracy, fixed facies colour identity and rendered appearance are not established by those checks.</p>
<p>Material balance, pressure-transient analysis, production forecasting and the separately packaged reservoir framework are outside this calculation. Broader historical examples request package exports that are absent from the current public initializer; those examples are not treated as executable evidence.</p>
</section>

<section id="section-4"><h2>4. Data and Basis</h2>
<p>The retained teaching brief supplies the input basis. Arithmetic is evaluated in field units; the oil formation volume factor is supplied at the same initial reservoir condition as the saturation estimate.</p>
<p>The example area is {v('area_acres')} acres and net pay is {v('net_pay_ft', ',.3f')} ft. Porosity is {v('porosity', '.2f')} and initial oil saturation is {v('oil_saturation', '.2f')}, both fractions. The initial oil formation volume factor is {v('formation_volume_factor', '.2f')} reservoir barrels per stock-tank barrel. Primary and raised recovery fractions are {v('primary_factor', '.2f')} and {v('raised_factor', '.2f')}. Source: retained teaching brief, Given table; input lineage is recorded in the appendix.</p>
<h3>4.1 Theory and criteria</h3>
<p>N = C A h φ S<sub>o</sub> / B<sub>oi</sub>; V(R) = N R; ΔV = N(R<sub>raised</sub> − R<sub>primary</sub>).</p>
<p>N is original oil in place (STB), C is the acre-foot conversion coefficient (barrels per acre-foot), A is area (acres), h is net pay (ft), φ is porosity (fraction), S<sub>o</sub> is initial oil saturation (fraction), B<sub>oi</sub> is initial oil formation volume factor (reservoir barrels per STB), R is recovery fraction, V is recoverable volume (STB), and ΔV is incremental recoverable volume (STB).</p>
<p>The implemented conversion coefficient is {v('coefficient')} barrels per acre-foot. Its stated precision and the volumetric and recovery equations match the open reserves reference, Volumetric estimation subsection, revision identified in <a href="#section-9">References</a>. The increment equation is the difference of the two recovery-volume calculations.</p>
<p>Finite nonnegative area and pay, fractions between zero and unity, and a positive formation volume factor define the caller input domain. For a physical recovery fraction, the recoverable volume is bounded by the original oil in place; this follows directly from its definition. It is not a recovery-performance acceptance criterion. The scalar module does not enforce these checks.</p>
<p>Uniform effective properties are assumed by the scalar representation. A homogeneous depiction is marked ASSUMED - to be confirmed for field use. A commercial recovery target, uncertainty range and field validation criterion are NOT_EVALUATED because no field basis is supplied.</p>
</section>

<section id="section-5"><h2>5. Detailed Results</h2>
<p>The documented teaching inputs are passed to the public volumetric functions. Each displayed example result is populated from the returned scalar rather than entered as report text.</p>
<p>Original oil in place is {v('oil_in_place')} STB. The primary scenario returns {v('primary')} STB at recovery fraction {v('primary_factor', '.2f')}. The raised scenario returns {v('raised')} STB at recovery fraction {v('raised_factor', '.2f')}. Incremental recovery is {v('increment')} STB.</p>
<p>Both scenarios lie below the oil-in-place bound of {v('oil_in_place')} STB. The difference is the additional volume implied by the selected fractions, with no time or cost assigned. Source: retained teaching inputs and the scalar volumetric functions; the matching regression cases are recorded in the appendix.</p>
</section>

<section id="section-6"><h2>6. Recovery Sensitivity</h2>
<p>The recoverable volume is varied against recovery fraction while the teaching oil volume is held fixed. Every sampled volume is calculated through the public recovery function.</p>
{recovery_chart(data)}
<p>The straight line expresses multiplication by a selected fraction. No sweep efficiency, relative permeability, well spacing or depletion history is inferred from it. The chosen recovery scenarios require a separate reservoir-performance basis before field adoption.</p>
</section>

<section id="section-7"><h2>7. Validation Evidence</h2>
<p>Current repository checks are used to assess arithmetic and interface behaviour. The recorded execution contains {counts['passed']} passing cases and {counts['skipped']} skipped cases, with no failed cases or collection errors; the complete scoped outcome record is identified in the reference register.</p>
<p>The volumetric checks cover a known-volume product, the direction of formation-volume-factor sensitivity, recovery multiplication and recovery increments. The teaching example checks oil in place and both primary and incremental recovery. Comparison is made to the repository's expected arithmetic values; it does not measure agreement against a field survey or an independent reservoir simulation.</p>
<p>Plotting checks use mocked graphics. Historical framework cases are skipped because the required exports are absent from the package initializer. Skipped cases provide no evidence for production forecasting or material balance.</p>
<p>No measured reservoir benchmark is present in the scoped evidence set. Field validation is NOT_EVALUATED. The documentation rebuild checks bind displayed values and chart samples to current module calls; they establish consistency of this report, not geological accuracy.</p>
</section>

<section id="section-8"><h2>8. Limits and Recommendations</h2>
<p>The scalar model is limited to a supplied static oil-bearing volume. Recovery fractions are user inputs; field performance and commercial classification are not calculated.</p>
<h3>8.1 Limits of use</h3>
<p>Heterogeneity, connected pay, free gas, fluid-property variation, aquifer support and time-dependent recovery are not resolved. A single effective saturation is insufficient to establish connected movable oil. Invalid fractions, nonfinite values or a nonpositive divisor are not rejected by the module; the calculation can return an unphysical value or raise an arithmetic error.</p>
<p>The public worked example demonstrates the arithmetic for its supplied inputs only. Uncertainty, production rates, operating constraints and commercial viability are NOT_EVALUATED. The input source is a retained teaching brief, with the original online statement unavailable.</p>
<h3>8.2 Recommendations</h3>
<p>Before field use, establish the mapped connected pay, representative porosity and saturation, and laboratory fluid-volume basis. Confirm the recovery fraction against reservoir-drive evidence and performance data. These actions follow the open reference's distinction between volumetric inventory and recovery efficiency.</p>
<p>Add caller validation for finite values, physical fractions and a positive divisor before using the scalar result in an engineering decision. Reject an incremental-recovery claim when its raised fraction is below the starting fraction. These recommendations follow the implemented arithmetic and fraction definition.</p>
<p>A proposed first extension is a validated input wrapper with boundary tests and a documented geological data register. Retain the current nominal arithmetic regression cases. Qualify recovery and stratigraphic interpretation separately against evidence suited to their intended use.</p>
</section>

<section id="section-9"><h2>9. References</h2>
<p>The equation basis and retained input record are distinguished below. Repository locators identify implementation and executable evidence; they are not external field-validation records.</p>
<h3>9.1 Published basis</h3>
<p><a href="{AAPG}">AAPG, Development Geology Reference Manual, Reserves estimation, Volumetric estimation, wiki revision 27160</a>, accessed 2026-10-21. The open edition states the conversion coefficient and oil-in-place and recovery equations. No licensed table or standard is reproduced.</p>
<p><a href="https://app.collide.io/posts/pe-problem-of-the-day-5-21-2026">Public petroleum-engineering teaching problem, 21 May 2026</a>, Given inputs, worked solution and recovery scenarios, retained repository edition. Original page unavailable at checking; no independently checked edition identifier is available.</p>
<h3>9.2 Repository evidence and reproduction</h3>
<p>Retained input record: <code>{BRIEF.as_posix()}</code>. Public calculation: <code>src/digitalmodel/reservoir/volumetrics.py</code>. Plotting interface: <code>src/digitalmodel/reservoir/stratigraphic.py</code>. Broader API status: <code>src/digitalmodel/reservoir/__init__.py</code> and <code>tests/test_reservoir_framework.py</code>.</p>
<p>Scoped execution record: <code>{EVIDENCE.as_posix()}</code>. Rebuild: <code>PYTHONPATH=src python scripts/reporting/generate_reservoir_report.py</code>. Report checks: <code>python -m pytest tests/reservoir/test_module_report.py</code>. The public calls are:</p>
<pre><code>from digitalmodel.reservoir.volumetrics import (
    VolumetricInputs, ooip_stb, recoverable_stb, incremental_recovery_stb,
)
# Inputs and recovery fractions are read from the cited Given table.
original = ooip_stb(VolumetricInputs(**documented_inputs))
primary = recoverable_stb(original, primary_factor)
raised = recoverable_stb(original, raised_factor)
increment = incremental_recovery_stb(original, primary_factor, raised_factor)</code></pre>
{source_table(data)}
</section>

<section id="section-10"><h2>10. Appendices</h2>
<p>The numerical lineage, qualification gaps and scoped check outcomes are retained below. The reader can distinguish supplied teaching inputs from missing field evidence.</p>
<h3>10.1 Data register</h3>
<table><thead><tr><th>Item</th><th>Origin and intended use</th><th>Qualification</th><th>Source</th></tr></thead><tbody>
<tr><td>Area, pay, porosity, saturation and fluid-volume factor</td><td>Retained public teaching brief; demonstration only</td><td>Documented example inputs; no measured field basis</td><td>Section 9.2, retained input record</td></tr>
<tr><td>Recovery fractions</td><td>Retained teaching scenarios; recovery multiplication</td><td>Scenario inputs; performance not established</td><td>Section 9.2, retained input record</td></tr>
<tr><td>Uniform effective properties and conceptual shape</td><td>Scalar idealisation and explanatory schematic</td><td>ASSUMED - to be confirmed for field use</td><td>Section 9.2, public calculation documentation</td></tr>
<tr><td>Mapped connected volume, laboratory fluid data and production history</td><td>Required field inputs</td><td>NOT_EVALUATED; records are not supplied</td><td>Section 9.1, volumetric and recovery basis</td></tr>
<tr><td>Commercial target and field-validation tolerance</td><td>Required decision criteria</td><td>NOT_EVALUATED; no numerical threshold is assigned</td><td>Section 9.1, distinction between inventory and recovery</td></tr>
</tbody><caption>Table 10.1. Data register. No missing field value is replaced by a guessed number.</caption></table>
<h3>10.2 Quality checks and limits of evidence</h3>
{validation_table(data)}
<p>Input and result units are stated in the basis. Source digests identify UTF-8 text with line endings normalized to LF. The report rebuild and interface checks cover numerical agreement, internal references, offline interaction, and comment export and import. The record above covers the existing module checks; it does not count the report checks as reservoir validation.</p>
<p>Comments are held in memory until saved. Use the save control and select a file beside this report. Where the browser offers download only, move the downloaded comments file beside the report. Load that file to continue a review; no automatic adjacent-file access is assumed.</p>
</section>
<details id="comments-panel"><summary>Comments on this report</summary>
<label for="comment-section">Section</label><select id="comment-section">{comment_options()}</select>
<label for="comment-text">Comment</label><textarea id="comment-text" rows="4" maxlength="10000"></textarea>
<button id="comment-add" type="button">Add comment</button>
<button id="comment-export" type="button">Save comments beside this report</button>
<label for="comment-import">Load saved comments</label><input id="comment-import" type="file" accept=".json,application/json">
<p id="comment-status" role="status">Comments remain in memory until saved.</p><ul id="comment-list"></ul></details>
</main></div>'''


def source_table(data: dict[str, Any]) -> str:
    """Keep source locators and LF-normalized text identities in the register."""
    rows = "".join(f'<tr><td><code>{escape(path)}</code></td><td><code>{sha}</code></td></tr>'
                   for path, sha in data["source_digests"].items())
    return ('<table><thead><tr><th>Record</th><th>SHA-256 digest</th></tr></thead>'
            f'<tbody>{rows}</tbody><caption>Table 9.1. SHA-256 identities of LF-normalized UTF-8 text for the report basis.</caption></table>')


def validation_table(data: dict[str, Any]) -> str:
    """Aggregate recorded outcomes by test file without inventing test claims."""
    groups: dict[str, dict[str, int]] = {}
    for case in data["evidence"]["cases"]:
        key = case["file"]
        groups.setdefault(key, {"passed": 0, "skipped": 0})[case["outcome"]] += 1
    labels = {"test_reservoir.py": "Package and public interface",
              "test_stratigraphic.py": "Stratigraphic display with mocked graphics",
              "test_volumetrics.py": "Volumetric arithmetic and teaching example",
              "test_reservoir_framework.py": "Historical framework with absent exports"}
    rows = "".join(f'<tr><td>{labels[Path(path).name]}</td><td>{counts["passed"]}</td>'
                   f'<td>{counts["skipped"]}</td><td>Section 9.2, scoped execution record</td></tr>'
                   for path, counts in groups.items())
    return ('<table><thead><tr><th>Evidence group</th><th>Passed (cases)</th><th>Skipped (cases)</th>'
            f'<th>Source</th></tr></thead><tbody>{rows}</tbody>'
            '<caption>Table 10.2. Existing module test outcomes. Skipped cases are not validation evidence.</caption></table>')


def comment_options() -> str:
    """Use section anchors rather than personal or environment identifiers."""
    return "".join(f'<option value="section-{i}">Section {i}</option>' for i in range(1, 11))


def build(output_dir: Path) -> dict[str, Any]:
    """Write deterministic HTML and the matching engineering-change sidecar."""
    data = calculation()
    data["evidence"] = evidence_record()
    sources = [BRIEF, Path("src/digitalmodel/reservoir/volumetrics.py"), EVIDENCE, STYLE_SOURCE]
    data["source_digests"] = {path.as_posix(): digest(path) for path in sources}
    data["style_source"] = STYLE_SOURCE.as_posix()
    style_match = re.search(r"<style>(.*?)</style>", (ROOT / STYLE_SOURCE).read_text(encoding="utf-8"), re.S)
    if style_match is None:
        raise ValueError("The existing engineering report stylesheet is missing")
    style = style_match.group(1)
    # Layout and palette are copied intact from the existing engineering report.
    extra = '''body.hs-doc{--report-accent:#137e8a}code{overflow-wrap:anywhere}pre{white-space:pre-wrap}
button,select,textarea,input{font:inherit;max-width:100%;margin:6px 0}textarea{display:block;width:100%}
details{margin:18px 0}summary{cursor:pointer}output{display:block}input[type=range]{width:100%}
button{padding:6px 12px}svg text{font:15px sans-serif}th,td{overflow-wrap:anywhere}
@media(max-width:600px){table{table-layout:fixed}nav ol{columns:1}main{overflow-wrap:anywhere}}'''
    content = report_body(data)
    page = f'''<!DOCTYPE html>
<html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">
<title>Reservoir volumetric analysis</title><style id="house-style">{style}</style><style>{extra}</style></head>
<body class="hs-doc">{content}
<script id="calculation-data" type="application/json">{json.dumps(data, ensure_ascii=False, allow_nan=False)}</script>
<script id="changes-data" type="application/json">{json.dumps(changes(), ensure_ascii=False)}</script>
<script>{INTERFACE}</script></body></html>
'''
    output_dir.mkdir(parents=True, exist_ok=True)
    (output_dir / "index.html").write_text(page, encoding="utf-8", newline="\n")
    (output_dir / "changes.json").write_text(json.dumps(changes(), indent=2) + "\n", encoding="utf-8", newline="\n")
    return data


def main() -> None:
    """Rebuild in place or into an explicit disposable output directory."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", type=Path, default=ROOT / DESTINATION)
    args = parser.parse_args()
    build(args.output_dir)
    print("Reservoir report and change panel rebuilt")


if __name__ == "__main__":
    main()
