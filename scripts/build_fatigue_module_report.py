# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Build the standalone fatigue module report from native calculation results.

Run with the repository on PYTHONPATH. The default output is the report folder;
--output-dir directs verification rebuilds outside the checkout. No network,
solver or report-rendering service is used during calculation or rebuilding.
"""

from __future__ import annotations

import argparse
import html
import json
import math
import re
from pathlib import Path

from digitalmodel.fatigue.assessment import AssessmentInput, assess_fatigue

ROOT = Path(__file__).resolve().parents[1]
DESTINATION = ROOT / "docs/reports/modules/fatigue"
STYLE_SOURCE = ROOT / "docs/api/cfd/turbulent-flat-plate-verification.html"
BRAND_SOURCE = ROOT / "docs/api/_assets/brand.css"
REVISION = "1"
DATE = "2026-10-21"


def display(value: float) -> str:
    """Render engineering quantities without hiding nonzero results."""
    if isinstance(value, int):
        return str(value)
    if value == 0:
        return "0.0"
    if abs(value) < 0.1 or abs(value) >= 10000:
        return f"{value:.1e}".replace("e+0", "e+").replace("e-0", "e−").replace("e-", "e−")
    return f"{value:,.1f}".replace("-", "−")


def calculate() -> dict:
    """Call the public assessment on the published signal and assumed basis."""
    fixture = json.loads((DESTINATION / "public-example.json").read_text(encoding="utf-8"))
    w, a = fixture["waveform"], fixture["assumptions"]
    times = [w["span"] * i / w["intervals"] for i in range(w["intervals"] + 1)]
    signal = [w["offset"] + w["sine_amplitude"] * math.sin(t)
              + w["cosine_amplitude"] * math.cos(w["cosine_frequency"] * t)
              + w["secondary_sine_amplitude"] * math.sin(w["secondary_sine_frequency"] * t)
              for t in times]
    stress = [value * a["stress_scale_mpa"] for value in signal]
    inp = AssessmentInput.model_validate({
        **{key: a[key] for key in ("sn_curve_name", "environment", "block_years",
                                  "design_years", "design_fatigue_factor")},
        "load_cases": [{"name": "Public waveform", "kind": "history",
                        "stress_history_mpa": stress}],
    })
    result = assess_fatigue(inp)
    exposure_at_limit = inp.block_years / (result.block_damage * inp.design_fatigue_factor)
    # These are plotting choices, not engineering constants or load cases.
    years = [exposure_at_limit * i / 10 for i in range(1, 13)]
    years += [inp.design_years, exposure_at_limit]
    exposure = [{"years": year, "utilisation": assess_fatigue(
        inp.model_copy(update={"design_years": year})).utilisation, "limit": 1.0}
        for year in sorted(set(years))]
    cycles = result.cases[0].cycles
    expected = fixture["published_range_counts"]
    counted = sorted(cycles, key=lambda c: c.stress_range_mpa)
    if len(expected) != len(counted):
        raise ValueError("public example cycle-row count differs")
    errors = []
    for native, public in zip(counted, expected):
        reference = public[0] * a["stress_scale_mpa"]
        if not math.isclose(native.stress_range_mpa, reference, rel_tol=1e-12):
            raise ValueError("public example stress-range distribution differs")
        if native.cycles != public[1]:
            raise ValueError("public example cycle distribution differs")
        errors.append(abs(native.stress_range_mpa - reference) / reference * 100)
    metrics = {
        "sample_count": len(times), "sample_span": w["span"],
        "stress_scale": a["stress_scale_mpa"], "block_years": inp.block_years,
        "design_years": inp.design_years, "fatigue_factor": inp.design_fatigue_factor,
        "cycle_total": sum(c.cycles for c in cycles),
        "maximum_range": max(c.stress_range_mpa for c in cycles),
        "block_damage": result.block_damage, "life_years": result.life_years,
        "utilisation": result.utilisation, "exposure_at_limit": exposure_at_limit,
        "acceptance_limit": 1.0,
        "count_difference_bound_percent": math.ceil(max(errors) * 10) / 10,
    }
    return {"input": inp.model_dump(), "result": result.model_dump(),
            "time": times, "stress": stress, "metrics": metrics,
            "exposure": exposure, "source": fixture["source"],
            "waveform": w, "assumptions": a}


def number(data: dict, key: str) -> str:
    """Bind each displayed worked-example scalar to its native metric."""
    return f'<span data-value="{key}">{display(data["metrics"][key])}</span>'


def chart(chart_id: str, xs: list, ys: list, xlabel: str, ylabel: str,
          units: str, limit: float | None = None) -> str:
    """Draw an offline SVG plot with keyboard and pointer accessible values."""
    left, right, top, bottom = 90, 820, 30, 300
    xmin, xmax = min(xs), max(xs)
    ymin = min(0, min(ys))
    ymax = max(max(ys), limit or 0)
    ymax += (ymax - ymin) * 0.08

    def xpixel(x):
        return left + (x - xmin) / (xmax - xmin) * (right - left)

    def ypixel(y):
        return bottom - (y - ymin) / (ymax - ymin) * (bottom - top)

    points = " ".join(f"{xpixel(x):.2f},{ypixel(y):.2f}" for x, y in zip(xs, ys))
    axis = []
    for i in range(5):
        x = xmin + (xmax - xmin) * i / 4
        y = ymin + (ymax - ymin) * i / 4
        axis.append(f'<text x="{xpixel(x):.2f}" y="325" text-anchor="middle">{display(x)}</text>')
        axis.append(f'<text x="80" y="{ypixel(y):.2f}" text-anchor="end">{display(y)}</text>')
    circles = []
    limit_text = f"{display(limit)} {units}" if limit is not None else "NOT_EVALUATED"
    for x, y in zip(xs, ys):
        tip = (f"Case: Public waveform; component: illustrative hot spot; "
               f"demand: {display(y)} {units}; limit: {limit_text}; "
               f"{xlabel}: {display(x)}; units: {units}")
        circles.append(f'<circle tabindex="0" cx="{xpixel(x):.2f}" cy="{ypixel(y):.2f}" '
                       f'r="4" fill="var(--brand)" aria-label="{html.escape(tip)}">'
                       f'<title>{html.escape(tip)}</title></circle>')
    limit_line = ""
    if limit is not None:
        limit_line = (f'<line x1="{left}" x2="{right}" y1="{ypixel(limit):.2f}" '
                      f'y2="{ypixel(limit):.2f}" stroke="var(--nogo)" stroke-dasharray="7 4"/>'
                      f'<text x="{right}" y="{ypixel(limit) - 8:.2f}" text-anchor="end">'
                      f'Utilisation limit {display(limit)}</text>')
    return f'''<figure id="{chart_id}">
<svg viewBox="0 0 880 375" role="group" aria-label="{html.escape(ylabel)} versus {html.escape(xlabel)}">
<line x1="{left}" x2="{right}" y1="{bottom}" y2="{bottom}" stroke="var(--mut)"/>
<line x1="{left}" x2="{left}" y1="{top}" y2="{bottom}" stroke="var(--mut)"/>
{''.join(axis)}{limit_line}
<polyline fill="none" stroke="var(--brand)" stroke-width="2" points="{points}"/>
{''.join(circles)}
<text x="455" y="365" text-anchor="middle">{html.escape(xlabel)}</text>
<text x="22" y="170" transform="rotate(-90 22 170)" text-anchor="middle">{html.escape(ylabel)}</text>
</svg><figcaption>Hover over a point or focus it with the keyboard to inspect the demand and its limit.</figcaption>
<output aria-live="polite"></output></figure>'''


SCHEMATIC = '''<figure id="system-sketch">
<svg viewBox="0 0 880 265" role="img" aria-labelledby="system-title system-desc">
<title id="system-title">Conceptual strip under cyclic axial stress</title>
<desc id="system-desc">Opposing axial stress arrows act on a welded strip. One surface location is evaluated.
The module receives stress values at that location; geometry, mesh and restraints are not solved.</desc>
<g fill="none" stroke="var(--brand)" stroke-width="2">
<path d="M180 85 L690 85 L690 155 L180 155 Z M430 85 L445 100 L430 115 L445 130 L430 155"/>
<path d="M180 120 L75 120 L95 110 M75 120 L95 130 M690 120 L795 120 L775 110 M795 120 L775 130"/>
<circle cx="440" cy="87" r="6"/><path d="M440 80 L500 40 L690 40"/>
</g>
<g fill="var(--ink)"><text x="505" y="30">Stress evaluation point</text>
<text x="60" y="100">Axial stress</text><text x="695" y="100">Axial stress</text>
<text x="295" y="190">Conceptual weld; dimensions are unspecified</text>
<text x="105" y="230">Input: local stress history. Output: counted ranges, damage and exposure utilisation.</text></g>
</svg><figcaption>Conceptual system and calculation boundary. A structural transfer model, mesh,
restraints and weld classification are outside this calculation.</figcaption></figure>'''


INTERACTION = r'''
<script>
"use strict";
// All engineering values are supplied by the calculation, never recomputed here.
for (const figure of document.querySelectorAll("figure")) {
  for (const point of figure.querySelectorAll("circle[aria-label]")) {
    const show = () => { figure.querySelector("output").textContent = point.getAttribute("aria-label"); };
    point.addEventListener("pointerenter", show);
    point.addEventListener("focus", show);
  }
}
const changeRecord = JSON.parse(document.getElementById("engineering-changes").textContent);
const changeList = document.getElementById("change-list");
for (const change of changeRecord.revisions) {
  const item = document.createElement("p");
  item.textContent = "Revision " + change.revision + " — " + change.date + ": " + change.description;
  changeList.appendChild(item);
}
let notes = [];
let folder = null;
const status = document.getElementById("comment-status");
const list = document.getElementById("comment-list");
function renderNotes() {
  list.replaceChildren();
  for (const note of notes) {
    const item = document.createElement("li");
    item.textContent = note.section + ": " + note.text;
    list.appendChild(item);
  }
}
function readNotes(text) {
  const value = JSON.parse(text);
  if (value.report !== "fatigue-module" || value.revision !== "1" || !Array.isArray(value.comments)
      || value.comments.some(n => typeof n.section !== "string" || typeof n.text !== "string")) {
    throw new Error("Comment file does not match this report and revision.");
  }
  return value.comments;
}
document.getElementById("add-comment").addEventListener("click", () => {
  const text = document.getElementById("comment-text");
  if (!text.value.trim()) return;
  notes.push({section: document.getElementById("comment-section").value, text: text.value.trim()});
  text.value = "";
  renderNotes();
  status.textContent = "Comment added in memory. Save comments before closing.";
});
document.getElementById("load-comments").addEventListener("change", async event => {
  try {
    const file = event.target.files[0];
    if (!file) return;
    notes = readNotes(await file.text());
    renderNotes();
    status.textContent = "Comments loaded. Save any further changes before closing.";
  } catch (error) { status.textContent = error.message; }
});
document.getElementById("save-comments").addEventListener("click", async () => {
  const content = JSON.stringify({report: "fatigue-module", revision: "1", comments: notes}, null, 2) + "\n";
  try {
    if (window.showDirectoryPicker) {
      if (!folder) folder = await window.showDirectoryPicker({mode: "readwrite"});
      await folder.getFileHandle("report.html");
      let previous = null;
      try { previous = await folder.getFileHandle("report.comments.json"); }
      catch (error) { if (error.name !== "NotFoundError") throw error; }
      if (previous && !window.confirm("Replace the existing report.comments.json with these comments?")) return;
      const file = previous || await folder.getFileHandle("report.comments.json", {create: true});
      const stream = await file.createWritable();
      await stream.write(content);
      await stream.close();
      status.textContent = "Comments saved beside report.html as report.comments.json.";
    } else {
      const url = URL.createObjectURL(new Blob([content], {type: "application/json"}));
      const link = document.createElement("a");
      link.href = url;
      link.download = "report.comments.json";
      link.click();
      setTimeout(() => URL.revokeObjectURL(url), 1000);
      status.textContent = "Comment download requested. Save report.comments.json beside report.html.";
    }
  } catch (error) { status.textContent = "Comments remain in memory; save failed: " + error.message; }
});
</script>
'''


def changes() -> dict:
    """Describe the engineering content introduced in this revision."""
    return {"report": "fatigue-module", "revisions": [
        {"revision": REVISION, "date": DATE,
         "description": "The load-block calculation basis, public counting comparison and assumed exposure example are introduced to define the scope of fatigue-life decisions."}
    ]}


def render(data: dict) -> str:
    """Write the complete engineering narrative against one calculation record."""
    n = lambda key: number(data, key)
    source = data["source"]
    result = data["result"]
    w = data["waveform"]
    style = re.search(r"<style>(.*?)</style>", STYLE_SOURCE.read_text(encoding="utf-8"), re.S)[1]
    style = BRAND_SOURCE.read_text(encoding="utf-8") + "\n" + style
    # Retain the canonical rules; remove editorial comments from the public page.
    style = re.sub(r"/\*.*?\*/", "", style, flags=re.S)
    style = "\n".join(line.rstrip() for line in style.splitlines())
    signal_chart = chart("stress-history", data["time"], data["stress"],
                         "Published sample coordinate (dimensionless)", "Local stress (MPa)", "MPa")
    exposure = data["exposure"]
    limit_chart = chart("exposure-envelope", [p["years"] for p in exposure],
                        [p["utilisation"] for p in exposure], "Assumed exposure (years)",
                        "Fatigue utilisation (dimensionless)", "dimensionless", 1.0)
    cycles = sorted(result["cases"][0]["cycles"], key=lambda c: c["stress_range_mpa"])
    cycle_rows = "\n".join(
        '<tr>' + ''.join(f'<td>{display(c[key])}</td>' for key in
                         ("stress_range_mpa", "cycles", "allowable_cycles", "damage"))
        + '<td>Native result; public counting example</td></tr>' for c in cycles)
    summary_verdict = ("The assumed exposure satisfies the calculation's utilisation limit."
                       if result["passed"] else "The assumed exposure exceeds the calculation's utilisation limit.")
    formula = (f"q(x) = {display(w['offset'])} + {display(w['sine_amplitude'])} sin(x) "
               f"+ {display(w['cosine_amplitude'])} cos({display(w['cosine_frequency'])}x) "
               f"+ {display(w['secondary_sine_amplitude'])} sin({display(w['secondary_sine_frequency'])}x)")
    changes_json = json.dumps(changes(), ensure_ascii=False).replace("<", "\\u003c")
    payload = json.dumps(data, ensure_ascii=False, allow_nan=False).replace("<", "\\u003c")
    section_options = ''.join(f'<option>{i}. {title}</option>' for i, title in enumerate(
        ["Executive summary", "Introduction", "Design data", "Analysis methodology", "Detailed results",
         "Key findings", "Limitations", "Recommendations", "References", "Acronyms"], 1))
    return f'''<!DOCTYPE html>
<html lang="en" data-theme="light">
<head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">
<title>Fatigue module analysis</title>
<style>{style}</style></head>
<body class="hs-doc"><div class="wrap">
<header><div class="kicker">Engineering analysis</div><h1>Fatigue module analysis</h1>
<p class="sub">Draft for client review</p></header>
<main>
<section id="summary"><h2>1. Executive summary</h2>
<p>The fatigue module is a stress-based fatigue assessment library. Analysis is performed to determine
cumulative damage and exposure utilisation at a supplied stress evaluation point, and the key results
are summarized below.</p>
<ul><li>The stress-history workflow is available for an explicitly defined load block.
<ul><li>Counted stress ranges, cumulative damage and duration-scaled life are returned together.</li>
<li>The public counting example is reproduced; the life result remains conditional on the assumed stress scale,
exposure and selected resistance curve.</li></ul></li>
<li>{summary_verdict}<ul><li>The boundary is a dimensionless utilisation of unity, including the supplied fatigue factor.</li>
<li>No component acceptance or service-life forecast is established from this illustrative loading.</li></ul></li></ul>
<h3>Revision history</h3>
<table><thead><tr><th>Revision</th><th>Date</th><th>Change and reason</th></tr></thead>
<tbody><tr><td>{REVISION}</td><td>{DATE}</td><td>The calculation basis and a public counting example are introduced to define the limits of use.</td></tr></tbody></table>
<p>The governing calculation boundary is shown in <a href="#exposure-envelope">the exposure chart</a>.
The assumed duration is extended only to illustrate the utilisation boundary; this is not an operating envelope for a physical asset.</p>
</section>
<section id="introduction"><h2>2. Introduction</h2>
<p>Fatigue damage is assessed from local stress histories or pre-counted stress ranges.
A single stress evaluation point under repeated loading is represented; water depth and global structural response are not represented.</p>
<p>The main workflow is a duration-aware load-block assessment. Ordered histories are counted without replacing
ranges by histogram-bin centres. Additive load cases are accumulated in one block; alternative operating scenarios are assessed separately.
Inputs, native outputs, existing numerical evidence and the engineering decisions that remain open are described below.</p>
</section>
<section id="design-data"><h2>3. Design data</h2>
<p>The calculation basis is supplied as local stress, material-detail selection and exposure duration.
The geometry and boundary conditions that produce the stress are established outside the module.</p>
<h3>3.1 System and stress boundary</h3>
<p>A conceptual welded strip is shown to locate the evaluation point. Neither a finite-element model nor an applied-force-to-stress conversion is included.</p>
{SCHEMATIC}
<h3>3.2 Inputs and outputs</h3>
<p>A history case contains a name and ordered stress samples in megapascals. A histogram case contains paired
nonnegative stress ranges and counts. The complete set of cases is assigned a positive block duration,
a positive design duration, a selected stress-life curve, an environment and a design fatigue factor.
Nonfinite values, unequal histogram lengths and duplicate case names are rejected by the assessment input contract
(<a href="#native-basis">native calculation basis</a>).</p>
<p>Outputs comprise per-range allowable cycles and damage, case contributions, total block damage,
unfactored life, factored utilisation, an acceptance flag and the largest case contribution. An unbounded life under zero damage
is returned as a null value; it is not a measured service life. Native export support also provides structured data,
a workbook and a report, with a portable document available only when a renderer is present. Export rendering is not exercised here.</p>
<h3>3.3 Resistance and criteria</h3>
<p>The demonstration selects class D in air from the native curve library. The library identifies DNV-RP-C203,
Fatigue design of offshore steel structures, with a 2021 label; its parameter tests record comparison with the October 2011 edition,
S-N curves in air. Equivalence to the later edition is not established by this report.
No licensed parameter table is reproduced (<a href="#curve-basis">curve evidence</a>).</p>
<p>The calculation criterion is utilisation less than or equal to unity after the supplied fatigue factor is applied.
This is the native assessment boundary, rather than a project-specific design approval criterion.
The factor, curve classification and environment remain engineer-supplied inputs. The counting comparison uses the public output distribution,
with exact count agreement and a relative range tolerance of one part in a trillion as a numerical comparison choice;
it is not a material or design tolerance (<a href="#report-qa">numerical checks</a>).</p>
<h3>3.4 Public waveform and assumptions</h3>
<p>The waveform and sample construction are taken from the Usage example of
<a href="#public-example">rainflow release 3.2.0</a>. Its coordinate and signal are dimensionless.
The sample span is {n('sample_span')} and the sample count is {n('sample_count')}, including both endpoints.</p>
<div class="eq">{formula}</div>
<p>The symbols x and q denote the published sample coordinate and signal ordinate respectively.
Every coefficient in this expression is taken from that public example; no metocean or material loading is implied.</p>
<p>ASSUMED - to be confirmed: local stress is the published signal multiplied by {n('stress_scale')} MPa per unit ordinate.
The complete record represents {n('block_years')} year of loading, the requested exposure is {n('design_years')} years,
and the design fatigue factor is {n('fatigue_factor')}.
These are demonstration inputs recorded in the <a href="#data-register">data register</a>, not values obtained from a physical installation.</p>
{signal_chart}
</section>
<section id="methodology"><h2>4. Analysis methodology</h2>
<p>Cycles are extracted by the native history counter and are evaluated by the native stress-life and cumulative-damage functions.
The validated assessment interface is called for both the worked result and every exposure-chart point.</p>
<h3>4.1 Counting and resistance</h3>
<p>Closed loops are counted as full cycles and adjacent residual reversals as half cycles.
The cycle range is the absolute difference between the two stress extrema; the cycle mean is their arithmetic mean.
The half-cycle convention is the repository's retained-residual policy and is checked against the open published example.
ASTM E1049-85, Standard Practices for Cycle Counting in Fatigue Analysis, Rainflow Counting,
is named by the implementation; its licensed clause text is not checked here.</p>
<div class="eq">N(S) = a / S<sup>m</sup></div>
<p>S is the local stress range in megapascals, N is allowable cycles, m is the selected segment slope,
and a is its intercept in cycles multiplied by megapascals raised to m. The native curve switches segments at its knee;
its second segment is anchored continuously at the knee. The knee is not an infinite-life cutoff in this assessment.
The catalogue endurance interface has a different cutoff convention and is not substituted for the native assessment curve
(<a href="#curve-basis">curve evidence</a>).</p>
<h3>4.2 Cumulative damage and exposure</h3>
<div class="eq">D = Σ n<sub>i</sub> / N(S<sub>i</sub>)<br>
L = T<sub>b</sub> / D<br>
U = D × (T<sub>d</sub> / T<sub>b</sub>) × F</div>
<p>D is dimensionless block damage, n is the counted cycles in range i, L is unfactored life in years,
T<sub>b</sub> is block duration in years, T<sub>d</sub> is requested exposure in years, F is the supplied design fatigue factor,
and U is dimensionless utilisation. The calculation passes when U does not exceed unity.
These equations are the implementation's linear cumulative-damage and duration-scaling model
(<a href="#native-basis">native calculation basis</a>).</p>
<p>Sequence effects, damage interaction and evolving material resistance are omitted by the linear sum.
Histogram-bin centres are avoided in this example because endurance is a nonlinear function of stress range.
The lower-level convenience life function has no explicit record duration and interprets reciprocal damage as years;
the explicit block-duration interface is used here to retain the exposure basis.</p>
</section>
<section id="results"><h2>5. Detailed results</h2>
<p>The public waveform is counted and its exact ranges are passed through the native assessment.
Results are interpreted only on the assumed stress scale, curve and duration basis.</p>
<h3>5.1 Public counting comparison</h3>
<p>The returned full-cycle equivalent count is {n('cycle_total')} cycles and the largest stress range is
{n('maximum_range')} MPa. Every range is within {n('count_difference_bound_percent')} % of the public reference,
with the percentage bound rounded upward from the unrounded differences; the corresponding counts agree exactly.
The open example qualifies counting for this one record; it supplies no independent material-life result.
The complete range and damage breakdown is retained in <a href="#cycle-table">the appendix table</a>.</p>
<h3>5.2 Native damage and utilisation</h3>
<p>Block damage is {n('block_damage')}. Unfactored mathematical life is {n('life_years')} years.
Requested-exposure utilisation is {n('utilisation')}, against a dimensionless limit of {n('acceptance_limit')}.
{summary_verdict} The reported life is an extrapolation of a sparse illustrative block and is not a service-life prediction.</p>
<p>The exposure at the factored utilisation boundary is {n('exposure_at_limit')} years.
The chart extends through that boundary to expose the limit and its exceedance, rather than to establish a permitted operating duration.</p>
{limit_chart}
</section>
<section id="findings"><h2>6. Key findings</h2>
<p>The native stress-history assessment is demonstrated end to end against a public counting distribution.
The design decision remains conditional on the supplied resistance and exposure inputs.</p>
<p>Exact counted ranges, per-range damage and explicit duration scaling are retained by the assessment interface.
Input validation and numerical regression checks provide evidence about implementation behaviour.
Physical validation of a welded component, a measured load spectrum or a design fatigue factor is not established by those checks.</p>
<p>The recorded library edition distinction and differing cutoff conventions are material to method selection.
A catalogue endurance value and the assessment's continuous low-stress branch are not interchangeable without an explicit design basis.</p>
</section>
<section id="limitations"><h2>7. Limitations</h2>
<p>Engineering acceptance is limited by the absence of a qualified component, measured loading and a confirmed resistance basis.
The public record demonstrates numerical counting rather than the fatigue strength of a real structure.</p>
<p>Mean stress is reported by counting but no mean-stress correction is applied by the selected assessment.
Thickness, stress concentration, hot-spot extrapolation, multiaxial loading, environment adjustments beyond the selected curve,
residual stress, overload interaction and corrosion evolution require separately justified treatment.
A generic thickness-correction default is not a universal detail-category basis.</p>
<h3>7.1 Not covered</h3>
<p>The wider package includes curve catalogues and fitting, stress concentration, hot-spot stress,
frequency-domain damage, weld classification, environmental corrections, multiaxial criteria, crack growth,
legacy spectral calculations and report exporters. These sub-areas are not qualified by this worked example.</p>
<p>The repository's ancillary evidence record identifies unresolved frequency-unit consistency in spectral rates,
an unsupported fallback for unknown weld details, and incomplete environment slope transfer.
Those are recorded concerns, not newly measured error magnitudes in this report; the related methods are excluded from its recommendations
(<a href="#ancillary-basis">ancillary evidence</a>).
Crack-growth inspection intervals, global structural response, dynamic motion, stability and physical operating envelopes are NOT_EVALUATED.</p>
</section>
<section id="recommendations"><h2>8. Recommendations</h2>
<p>The duration-aware assessment is recommended for numerical evaluation after the local stress and resistance basis is established.
Component acceptance is withheld until the open engineering inputs are resolved.</p>
<p>Supply a qualified weld-detail classification, a selected standard edition and environment, the represented block duration,
the required design exposure and the design fatigue factor before using the result for disposition.
Confirm the need for thickness, hot-spot and mean-stress corrections independently and supply a stress basis consistent with the chosen curve
(<a href="#native-basis">input contract</a>; <a href="#curve-basis">resistance evidence</a>).</p>
<p>When utilisation exceeds unity, reduce the assessed stress exposure or revise the component detail and repeat the assessment
against the same limit. No retrofit date or inspection trigger is specified without a qualified physical model and inspection basis.
Resolve the ancillary evidence concerns and add independent reference cases before extending this report's scope to those methods
(<a href="#ancillary-basis">recorded method limits</a>).</p>
</section>
<section id="references"><h2>9. References</h2>
<p>Sources and repository evidence are identified with their scope of support.
Licensed standard labels are cited for orientation; their clause text and numerical parameter tables are not reproduced.</p>
<h3>9.1 Public and normative references</h3>
<ul><li id="public-example"><a href="{source['url']}">rainflow, release 3.2.0, Usage</a>:
sample construction and published range-count distribution. The same release description is available as
<a href="{source['open_record']}">open package metadata</a>. The waveform coefficients and counting reference are read from that record.</li>
<li>DNV-RP-C203, Fatigue design of offshore steel structures, October 2011, S-N curves in air;
2021-labelled repository implementation, S-N curves in air. Later-edition equivalence is not checked here.</li>
<li>ASTM E1049-85, Standard Practices for Cycle Counting in Fatigue Analysis, Rainflow Counting.
This is an implementation citation; independent standards conformance is not claimed.</li></ul>
<h3>9.2 Appendix: evidence, data register and calculation record</h3>
<p>Repository locators and detailed calculation data are provided here so that the limits of each claim are explicit.</p>
<p>The public input mapping is validated by <code>AssessmentInput.model_validate(mapping)</code>,
then evaluated by <code>assess_fatigue(input)</code> from <code>digitalmodel.fatigue.assessment</code>.
The reproduction script constructs that mapping from the adjacent public-example fixture and calls these functions;
no fatigue result is entered manually.</p>
<table><caption>Evidence register. Existing tests are numerical regression evidence, not component certification.</caption>
<thead><tr><th>Evidence</th><th>Repository locator</th><th>Supported conclusion and limit</th></tr></thead><tbody>
<tr id="native-basis"><td>Input and equations</td><td><code>src/digitalmodel/fatigue/assessment.py</code>;
<code>src/digitalmodel/fatigue/damage.py</code>;
<code>docs/skills/fatigue-assessment/SKILL.md</code></td><td>Explicit durations, additive cases, linear damage and utilisation boundary; corrections are engineer supplied.</td></tr>
<tr id="curve-basis"><td>Resistance and cutoff</td><td><code>src/digitalmodel/fatigue/sn_curves.py</code>;
<code>src/digitalmodel/fatigue/sn_library.py</code>;
<code>tests/fatigue/test_c203_sn_values.py</code></td><td>Table-derived regression records use the earlier edition. Native low-stress continuation differs from the catalogue cutoff. No new standards qualification is claimed.</td></tr>
<tr id="evidence"><td>Existing main-workflow checks</td><td><code>tests/fatigue/test_rainflow.py</code>;
<code>tests/fatigue/test_damage.py</code>;
<code>tests/fatigue/test_sn_curves.py</code>;
<code>tests/fatigue/test_assessment.py</code></td><td>Signal counting, histogram preservation, damage scaling, duration validation and native output behaviour are asserted. Some exporter checks require optional packages.</td></tr>
<tr><td>Hand-derived counting fixture</td><td><code>src/digitalmodel/fatigue/counting_contract.py</code>, CONTRACT_EXACT;
<code>tests/fatigue/test_rainflow_invariants.py</code></td><td>A committed reversal trace supplies a separate arithmetic check. The wider registry also records blocked nonconforming damage routes.</td></tr>
<tr id="ancillary-basis"><td>Ancillary concerns</td><td><code>src/digitalmodel/fatigue/docs/agent-pack.md</code>, findings and open items;
<code>src/digitalmodel/fatigue/spectral_fatigue.py</code>;
<code>src/digitalmodel/fatigue/weld_classification.py</code>;
<code>src/digitalmodel/fatigue/environmental_correction.py</code></td><td>Open method concerns are retained as exclusions; their error magnitudes are not remeasured here.</td></tr>
<tr><td>Public example and rebuild evidence</td><td><code>docs/reports/modules/fatigue/public-example.json</code>;
<code>scripts/build_fatigue_module_report.py</code>;
<code>tests/fatigue/test_module_report.py</code></td><td>The native result, displayed values, exposure points and deterministic rebuild are checked against direct calls and the published counting distribution.</td></tr>
</tbody></table>
<table id="data-register"><caption>Data register. Missing physical data does not inherit demonstration assumptions.</caption>
<thead><tr><th>Input</th><th>Basis</th><th>Status and action</th></tr></thead><tbody>
<tr><td>Waveform coefficients, sample spacing and reference counts</td><td>Public release Usage example</td><td>Read from the open record; dimensional stress is not provided there.</td></tr>
<tr><td>Stress scale, durations, fatigue factor, class D and air</td><td>Explicit demonstration inputs in the public-example fixture</td><td>ASSUMED - to be confirmed. Replace with a qualified engineering basis before disposition.</td></tr>
<tr><td>Geometry, material, weld profile, thickness and boundary conditions</td><td>No component data supplied</td><td>NOT_EVALUATED. Supply a structural stress model and resistance classification.</td></tr>
<tr><td>Physical load spectrum, environment history and inspection basis</td><td>No measured data supplied</td><td>NOT_EVALUATED. Establish representative exposure and condition evidence.</td></tr>
</tbody></table>
<table id="cycle-table"><caption>Native cycle breakdown on the assumed stress scale. Counts retain residual half cycles; damage is dimensionless.</caption>
<thead><tr><th>Range (MPa)</th><th>Cycles</th><th>Allowable cycles</th><th>Block damage</th><th>Source</th></tr></thead>
<tbody>{cycle_rows}</tbody></table>
<table id="report-qa"><caption>Numerical pre-flight and its evidence. These checks do not qualify a physical component.</caption>
<thead><tr><th>Check</th><th>Evidence</th><th>Criterion and scope</th></tr></thead><tbody>
<tr><td>Public counting reference</td><td>Published range-count distribution versus direct native counting</td><td>Counts match exactly; ranges meet the stated relative numerical tolerance.</td></tr>
<tr><td>Damage and exposure arithmetic</td><td>Direct assessment and independent per-range damage sum</td><td>Native result and reported inputs agree; chart points come from direct duration-aware calls.</td></tr>
<tr><td>Rebuild and scalar presentation</td><td>Rebuild test and displayed-value bindings</td><td>Committed HTML and changes record match regenerated bytes; every scalar is rendered from the calculation record.</td></tr>
<tr><td>Plot and review delivery</td><td>Inline vector charts, source references, comments and engineering change record</td><td>Interactive point inspection is available offline. Comment saving requires an explicit file permission or a sidecar download.</td></tr>
</tbody></table>
</section>
<section id="acronyms"><h2>10. Acronyms</h2>
<p>Notation and units are defined to support interpretation of the calculation.
Unexplained internal labels are excluded from the engineering narrative.</p>
<p>MPa denotes megapascal. S-N denotes stress range versus allowable cycle count.
The symbols D, L, U, F, n, N, S, a, m, T<sub>b</sub> and T<sub>d</sub> are defined in the methodology.
NOT_EVALUATED means that the stated engineering result is not established from the available inputs.</p>
</section>
</main>
<footer>
<details><summary>Engineering changes</summary><div id="change-list"></div></details>
<details><summary>Comments</summary>
<p>Comments are saved separately from the report. Load an existing sidecar before adding to it.
Save beside report.html as report.comments.json; where folder access is unavailable, place the downloaded file beside the report.</p>
<label for="load-comments">Load comments</label><input id="load-comments" type="file" accept="application/json,.json">
<label for="comment-section">Section</label><select id="comment-section">{section_options}</select>
<label for="comment-text">Comment</label><textarea id="comment-text" rows="4"></textarea>
<button id="add-comment" type="button">Add comment</button><button id="save-comments" type="button">Save comments beside report</button>
<p id="comment-status" role="status">No comments are saved until Save comments is selected.</p><ol id="comment-list"></ol>
</details>
</footer>
<script type="application/json" id="engineering-changes">{changes_json}</script>
<script type="application/json" id="calculation-record">{payload}</script>
{INTERACTION}
</div></body></html>
'''


def build(output_dir: Path = DESTINATION) -> None:
    """Write the report and its engineering changes, without other artifacts."""
    data = calculate()
    output_dir.mkdir(parents=True, exist_ok=True)
    (output_dir / "report.html").write_text(render(data), encoding="utf-8", newline="\n")
    (output_dir / "changes.json").write_text(
        json.dumps(changes(), indent=2, ensure_ascii=False) + "\n", encoding="utf-8", newline="\n")


def main() -> None:
    """Accept an optional verification output directory."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", type=Path, default=DESTINATION)
    args = parser.parse_args()
    build(args.output_dir)


if __name__ == "__main__":
    main()
