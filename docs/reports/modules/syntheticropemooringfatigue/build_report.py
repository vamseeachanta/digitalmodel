# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.
"""Build the single-file synthetic rope screening report from the public example.

Run from the repository root, with PYTHONPATH=src:
    uv run --no-project --with pyyaml python \
        docs/reports/modules/syntheticropemooringfatigue/build_report.py \
        --scratch <scratch>

Only the HTML and changes panel are deliverables. Workflow CSV files are written
under the required scratch argument. No engine or solver integration is imported.
"""

from __future__ import annotations

import argparse
import copy
import csv
import json
import math
import re
from html import escape
from pathlib import Path

import yaml

from digitalmodel.synthetic_rope_mooring_fatigue.workflow import router


ROOT = Path(__file__).resolve().parents[4]
HERE = Path(__file__).resolve().parent
EXAMPLE = ROOT / "examples/workflows/synthetic-rope-mooring-fatigue/input.yml"
LABELS = ("First polyester line", "Second polyester line")
MECHANISMS = {
    "fatigue": ("Tension-range fatigue", "fatigue_life_years", "fatigue_required_years", "years"),
    "creep": ("Creep rupture", "creep_life_years", "creep_required_years", "years"),
    "compression": ("Minimum tension", "min_tension_kN", "min_required_kN", "kN"),
}
CHANGES = {
    "document": "Synthetic rope mooring fatigue analysis",
    "revisions": [
        {
            "revision": "Initial",
            "date": "2026-10-21",
            "changes": [
                "The public two-line screening example is calculated through the module.",
                "Fatigue, creep and minimum-tension margins are separated from material qualification.",
                "Input provenance, limits of use and actions for the creep shortfall are recorded.",
            ],
        },
        {
            "revision": "1",
            "date": "2026-10-22",
            "changes": [
                "Model inputs and their unqualified source basis are exposed so the calculation can be checked.",
                "Verdicts follow the calculated mechanism, zero margins are plotted explicitly and boundary precision preserves failures.",
                "Independent fatigue arithmetic and the current workflow and atlas checks extend the evidence.",
            ],
        },
    ],
}


def display(value: float, key: str = "") -> str:
    """Use engineering precision without displaying small nonzero values as zero."""
    if not math.isfinite(value):
        raise ValueError("Report values must be finite")
    if key.startswith("parameter."):
        return str(value)
    if key.endswith("n_cycles"):
        return f"{value:,.0f}" if value == int(value) else f"{value:,}"
    if key.endswith("_life_years"):
        return f"{value:.3f}" if 0.001 <= abs(value) < 1000 else f"{value:.3g}"
    if key.endswith("_margin"):
        rounded = f"{value:.3g}"
        if 0 < value < 1 and float(rounded) >= 1:
            return "< 1.00"
        return f"{value:.2f}" if value == 1 else rounded
    if value == 0:
        return "0.0"
    magnitude = abs(value)
    if magnitude < 1 or magnitude >= 1_000_000:
        return f"{value:.4g}".replace("e-", "e−")
    return f"{value:,.1f}"


def calculate(scratch: Path) -> tuple[dict, dict, list[dict], dict[str, float]]:
    """Call the module on the unchanged engineering inputs; redirect outputs."""
    cfg = yaml.safe_load(EXAMPLE.read_text(encoding="utf-8"))
    inputs = copy.deepcopy(cfg["synthetic_rope_mooring_fatigue"])
    cfg["synthetic_rope_mooring_fatigue"]["output_dir"] = str(scratch.resolve())
    cfg["_config_file_path"] = str(EXAMPLE)
    summary = router(cfg)["synthetic_rope_mooring_fatigue"]
    with Path(summary["results_csv"]).open(encoding="utf-8", newline="\n") as stream:
        bins = list(csv.DictReader(stream))
    values = {"design_life": inputs["design_life_years"]}
    for group in ("dff", "creep_safety_factor", "tn_curve", "creep", "min_tension"):
        value = inputs[group]
        if isinstance(value, dict):
            values.update((f"parameter.{group}.{key}", item) for key, item in value.items())
        else:
            values[f"parameter.{group}"] = value
    for index, (source, result) in enumerate(zip(inputs["lines"], summary["lines"])):
        prefix = f"line.{index}."
        for field in ("MBL_kN", "mean_tension_kN", "min_tension_kN"):
            values[prefix + field] = source[field]
        values.update(
            (prefix + field, value)
            for field, value in result.items()
            if type(value) in (int, float)
        )
        values[prefix + "fatigue_required_years"] = inputs["design_life_years"] * inputs["dff"]
        values[prefix + "creep_required_years"] = inputs["design_life_years"] * inputs["creep_safety_factor"]
        values[prefix + "min_required_kN"] = source["MBL_kN"] * inputs["min_tension"]["min_ratio_allow"]
        line_bins = [row for row in bins if row["line_id"] == result["line_id"]]
        for j, row in enumerate(line_bins):
            for field in ("tension_range_kN", "n_cycles", "normalised_range", "allowable_cycles", "damage"):
                values[prefix + f"bin.{j}." + field] = float(row[field])
    return inputs, summary, bins, values


def number(key: str, values: dict[str, float]) -> str:
    """Mark every worked-example number for the rebuild test."""
    return f'<span data-number="{key}">{escape(display(values[key], key))}</span>'


def mechanism_statement(index: int, mechanism: str, values: dict) -> str:
    """State a decision with its actual value, requirement and input basis."""
    title, actual, required, units = MECHANISMS[mechanism]
    prefix = f"line.{index}."
    n = lambda field: number(prefix + field, values)
    status = "is met" if values[prefix + mechanism + "_margin"] >= 1 else "is not met"
    return (
        f"{LABELS[index]}: {title} criterion {status}; calculated {n(actual)} {units} "
        f"against {n(required)} {units} required, margin {n(mechanism + '_margin')} "
        'against unity (1.000). Basis: <a href="#table-parameters">supplied model parameters</a> '
        'and <a href="#table-inputs">prescribed loads</a>.'
    )


def verdicts(summary: dict, values: dict) -> tuple[str, str]:
    """Derive repeated conclusions and the next action from the governing result."""
    statements = []
    for i, line in enumerate(summary["lines"]):
        mechanism = line["governing_mechanism"]
        title = MECHANISMS[mechanism][0]
        statements.append(f"{title} governs. " + mechanism_statement(i, mechanism, values))
    governing = min(range(len(summary["lines"])), key=lambda i: values[f"line.{i}.line_margin"])
    mechanism = summary["lines"][governing]["governing_mechanism"]
    passes = all(values[f"line.{i}.{m}_margin"] >= 1 for i in range(len(LABELS)) for m in MECHANISMS)
    status = "meets" if passes else "does not meet"
    overall = f'Overall screening {status} the supplied criteria. ' + " ".join(statements)
    if passes:
        action = "No screening shortfall is calculated; material qualification and a site exposure basis remain required before service use."
    else:
        actions = {
            "fatigue": "Qualify the tension-range curve and exposure histogram, then reassess cyclic demand and breaking capacity.",
            "creep": "Obtain qualified supplier creep data at the intended load, temperature and environment, then reassess mean tension and breaking capacity.",
            "compression": "Obtain a qualified low-tension criterion, then reassess pretension and minimum tension against the prescribed threshold.",
        }
        action = f"{LABELS[governing]} requires reassessment for {MECHANISMS[mechanism][0].lower()}. " + actions[mechanism]
    return f'<p data-verdict="overall">{overall}</p>', action


def table(headers: tuple[str, ...], rows: list[list[str]], caption: str, ident: str) -> str:
    """Render real table columns and a caption with an addressable source."""
    head = "".join(f'<th scope="col">{item}</th>' for item in headers)
    body = "".join("<tr>" + "".join(f"<td>{item}</td>" for item in row) + "</tr>" for row in rows)
    return f'<table id="{ident}"><caption>{caption}</caption><thead><tr>{head}</tr></thead><tbody>{body}</tbody></table>'


def chart(mechanism: str, values: dict[str, float]) -> str:
    """Draw margins against line identity with a unity limit and bound hovers."""
    title, demand, required, units = MECHANISMS[mechanism]
    margins = [values[f"line.{index}.{mechanism}_margin"] for index in range(len(LABELS))]
    if not all(math.isfinite(margin) for margin in margins):
        raise ValueError("Chart margins must be finite")
    lower = min(-0.15, math.log10(min([m for m in margins if m > 0] + [1.0])) - 0.25)
    upper = max(0.15, math.log10(max(margins + [1.0])) + 0.25)

    def y(value):
        return 215 if value <= 0 else 215 - (math.log10(value) - lower) / (upper - lower) * 160

    limit_y = y(1.0)
    points = []
    for index, margin in enumerate(margins):
        prefix = f"line.{index}."
        x, cy = 235 + index * 320, y(margin)
        colour = "var(--go)" if margin >= 1 else "var(--nogo)"
        points.append(
            f'<g tabindex="0" role="button" aria-label="{LABELS[index]}, {title}; select for values" '
            f'data-point="{prefix}{mechanism}_margin" data-case="{LABELS[index]}" '
            f'data-component="Synthetic rope span" data-demand="{prefix}{demand}" '
            f'data-required="{prefix}{required}" data-units="{units}">'
            f'<circle cx="{x}" cy="{cy:.1f}" r="8" fill="{colour}"/>'
            f'<text x="{x}" y="{cy - 15:.1f}" text-anchor="middle" '
            f'data-number="{prefix}{mechanism}_margin">{escape(display(margin, prefix + mechanism + "_margin"))}</text>'
            f'<text x="{x}" y="250" text-anchor="middle">{LABELS[index]}</text></g>'
        )
    return (
        f'<figure class="plotbox" id="chart-{mechanism}"><svg viewBox="0 0 800 280" '
        f'role="img" aria-labelledby="title-{mechanism}" data-limit="1">'
        f'<title id="title-{mechanism}">{title}: margin by line, logarithmic vertical scale</title>'
        f'<text x="25" y="25">{title}: margin by line</text>'
        f'<text x="25" y="47">Logarithmic margin scale; higher is further above the supplied limit.</text>'
        f'<path d="M90 55V215H730" stroke="var(--ink)" fill="none"/>'
        f'<path d="M90 {limit_y:.1f}H730" stroke="var(--nogo)" stroke-dasharray="6 4"/>'
        f'<text x="95" y="{limit_y - 8:.1f}">Unity limit</text>'
        + "".join(points)
        + '</svg><figcaption>Hover, focus or select a marker for case, component, calculated life or tension, '
        'required life or tension, and units. The dashed line is the supplied acceptance limit. '
        'Zero or negative margins are shown at the lower plotting boundary with their actual values. '
        '<a href="#criteria">Basis: acceptance criteria.</a></figcaption></figure>'
    )


SCHEMATIC = """
<figure id="mooring-schematic">
<svg viewBox="0 0 800 360" role="img" aria-labelledby="mooring-title mooring-desc">
<title id="mooring-title">Synthetic rope mooring and screening boundary</title>
<desc id="mooring-desc">A floating body transfers tension through a fairlead and end fitting to a submerged synthetic rope span and seabed anchor. Prescribed tension bins replace the global motion model.</desc>
<path d="M15 112H785" stroke="var(--brand)" fill="none"/>
<path d="M80 70H310L275 125H115Z" fill="var(--panel-2)" stroke="var(--ink)"/>
<path d="M145 70V35H245V70M290 112L338 160" stroke="var(--ink)" fill="none"/>
<path d="M338 160Q435 270 615 300" stroke="var(--brand)" stroke-width="8" fill="none"/>
<path d="M615 300L657 310M15 320H785M642 310H678M657 290V315" stroke="var(--ink)" stroke-width="3" fill="none"/>
<circle cx="338" cy="160" r="7" fill="var(--panel)" stroke="var(--ink)"/>
<circle cx="615" cy="300" r="7" fill="var(--panel)" stroke="var(--ink)"/>
<text x="90" y="25">Floating body</text><text x="340" y="100">Water surface</text>
<text x="350" y="145">Fairlead and upper end fitting</text>
<text x="90" y="210">Synthetic rope span</text><path d="M245 207L395 220" stroke="var(--ink)"/>
<text x="60" y="245">Boundary inputs: mean tension, minimum tension,</text>
<text x="60" y="268">tension-range bins and cycle counts.</text>
<text x="530" y="345">Lower end fitting and anchor</text>
<text x="25" y="345">Seabed</text>
</svg>
<figcaption>The physical load path is shown schematically, without a geometric scale.
Only the synthetic span is screened. End fittings, chain tails, anchor capacity,
water depth, span length and host motion are not represented in the algebraic calculation.
Prescribed loads are applied at the span boundary; no finite-element mesh is used.</figcaption>
</figure>
"""


INTERACTION = r"""
(() => {
  'use strict';
  const values = JSON.parse(document.getElementById('numeric-record').textContent);
  const tip = document.getElementById('chart-readout');
  document.querySelectorAll('[data-point]').forEach(point => {
    const show = () => {
      const d = point.dataset;
      tip.textContent = `Case: ${d.case}; component: ${d.component}; demand (calculated life or minimum tension): ${values[d.demand]} ${d.units}; limit (required life or minimum tension): ${values[d.required]} ${d.units}; margin: ${values[d.point]} dimensionless.`;
    };
    ['mouseenter', 'focus', 'click'].forEach(event => point.addEventListener(event, show));
  });
  const changes = JSON.parse(document.getElementById('change-record').textContent);
  const panel = document.getElementById('changes-panel');
  changes.revisions.forEach(revision => {
    const paragraph = document.createElement('p');
    paragraph.textContent = `${revision.revision} — ${revision.date}: ${revision.changes.join(' ')}`;
    panel.appendChild(paragraph);
  });
  const state = {comments: []};
  let directory = null;
  const status = document.getElementById('comment-status');
  const list = document.getElementById('comment-list');
  const sidecar = 'report.comments.json';
  function draw() {
    list.replaceChildren();
    state.comments.forEach(comment => {
      const item = document.createElement('li');
      item.textContent = `${comment.section}: ${comment.text}`;
      list.appendChild(item);
    });
  }
  function load(text) {
    const data = JSON.parse(text);
    if (data.document !== 'synthetic-rope-mooring-fatigue' || !Array.isArray(data.comments) ||
        !data.comments.every(c => typeof c.section === 'string' && typeof c.text === 'string')) {
      throw new Error('The comments file does not match this report.');
    }
    state.comments = data.comments;
    draw();
  }
  document.getElementById('comment-form').addEventListener('submit', event => {
    event.preventDefault();
    const text = document.getElementById('comment-text');
    if (!text.value.trim()) return;
    state.comments.push({section: document.getElementById('comment-section').value, text: text.value.trim()});
    text.value = '';
    draw();
    status.textContent = 'Comments have changed. Save the comments file to keep them.';
  });
  document.getElementById('comment-folder').addEventListener('click', async () => {
    if (!window.showDirectoryPicker) {
      status.textContent = 'Folder access is unavailable. Export the comments file and place it beside this report; import it when reopening.';
      return;
    }
    try {
      const selected = await window.showDirectoryPicker({mode: 'readwrite'});
      await selected.getFileHandle('report.html');
      directory = selected;
      // Do not replace unsaved notes when selecting a folder.
      if (state.comments.length) {
        status.textContent = 'The report folder is selected. Save the current comments before importing other notes.';
        return;
      }
      try {
        const handle = await directory.getFileHandle(sidecar);
        load(await (await handle.getFile()).text());
        status.textContent = 'Comments were loaded from beside the report.';
      } catch (error) {
        if (error.name !== 'NotFoundError') throw error;
        status.textContent = 'The report folder is selected. Save comments to create the file beside the report.';
      }
    } catch (error) { status.textContent = `The folder was not selected: ${error.message}`; }
  });
  document.getElementById('comment-save').addEventListener('click', async () => {
    const text = JSON.stringify({document: 'synthetic-rope-mooring-fatigue', revision: 'Initial', comments: state.comments}, null, 2) + '\n';
    try {
      let handle;
      if (directory) handle = await directory.getFileHandle(sidecar, {create: true});
      else if (window.showSaveFilePicker) handle = await window.showSaveFilePicker({suggestedName: sidecar});
      if (handle) {
        const stream = await handle.createWritable();
        await stream.write(text); await stream.close();
        status.textContent = 'The comments file was saved. Keep it beside the report.';
      } else {
        const url = URL.createObjectURL(new Blob([text], {type: 'application/json'}));
        const link = document.createElement('a'); link.href = url; link.download = sidecar; link.click();
        setTimeout(() => URL.revokeObjectURL(url), 1000);
        status.textContent = 'The comments file was exported. Place it beside the report.';
      }
    } catch (error) { status.textContent = `Comments were not saved: ${error.message}`; }
  });
  document.getElementById('comment-import').addEventListener('change', async event => {
    try {
      if (state.comments.length && !window.confirm('Importing replaces the displayed comments. Save them first if needed. Continue?')) return;
      const file = event.target.files[0]; if (file) load(await file.text());
      status.textContent = 'Comments were imported. Save after making changes.';
    } catch (error) { status.textContent = `Comments were not imported: ${error.message}`; }
  });
})();
"""


def build(scratch: Path) -> tuple[str, dict]:
    """Write the complete report narrative with the full workflow in context."""
    inputs, summary, bins, values = calculate(scratch)
    # Reject invalid numerical returns before producing a decision or a chart.
    for value in values.values():
        if not math.isfinite(value):
            raise ValueError("Report inputs and results must be finite")
    overall, action = verdicts(summary, values)
    n = lambda key: number(key, values)
    input_rows = []
    result_rows = []
    bin_rows = []
    for i, label in enumerate(LABELS):
        prefix = f"line.{i}."
        input_rows.append([label, n(prefix + "MBL_kN"), n(prefix + "mean_tension_kN"), n(prefix + "min_tension_kN"), '<a href="#record-example">Public example</a>'])
        line = summary["lines"][i]
        result_rows.append([
            label, n(prefix + "total_damage"), n(prefix + "fatigue_life_years"),
            n(prefix + "fatigue_margin"), n(prefix + "creep_life_years"),
            n(prefix + "creep_margin"), n(prefix + "min_tension_ratio"),
            n(prefix + "compression_margin"), n(prefix + "line_margin"),
            MECHANISMS[line["governing_mechanism"]][0],
            mechanism_statement(i, line["governing_mechanism"], values),
            '<a href="#method">Module calculation</a>',
        ])
        for j, _ in enumerate(inputs["lines"][i]["tension_range_bins"]):
            base = prefix + f"bin.{j}."
            bin_rows.append([label] + [n(base + field) for field in ("tension_range_kN", "n_cycles", "normalised_range", "allowable_cycles", "damage")] + ['<a href="#record-example">Public example and module calculation</a>'])
    input_table = table(
        ("Synthetic span", "Breaking load (kN)", "Mean tension (kN)", "Minimum tension (kN)", "Source"),
        input_rows, "Table A. Prescribed physical inputs; these are demonstration data, not a site design.", "table-inputs",
    )
    result_table = table(
        ("Synthetic span", "Damage (dimensionless)", "Fatigue life (years)", "Fatigue margin (dimensionless)", "Creep life (years)", "Creep margin (dimensionless)", "Minimum-tension ratio (dimensionless)", "Minimum-tension margin (dimensionless)", "Governing margin (dimensionless)", "Governing mechanism", "Result", "Source"),
        result_rows, "Table B. Calculated line results for the prescribed exposure and supplied material laws.", "table-results",
    )
    bin_table = table(
        ("Synthetic span", "Tension range (kN)", "Exposure (cycles)", "Normalised range (dimensionless)", "Allowable cycles", "Damage (dimensionless)", "Source"),
        bin_rows, "Table C. All prescribed tension bins and calculated cycle capacities; range is peak to trough, not amplitude.", "table-bins",
    )
    requirements = table(
        ("Synthetic span", "Required fatigue life (years)", "Required creep life (years)", "Required minimum tension (kN)", "Source"),
        [[label] + [n(f"line.{i}." + field) for field in ("fatigue_required_years", "creep_required_years", "min_required_kN")] + ['<a href="#criteria">Supplied acceptance criteria</a>'] for i, label in enumerate(LABELS)],
        "Table D. Acceptance limits calculated from the public example inputs.", "table-limits",
    )
    attributed = 'Public input attribution to <a href="#ref-standard">DNV-OS-E301, October 2010, Table F3/F702</a>; primary text not opened; NOT_EVALUATED.'
    unsourced = "Unsourced demonstration value; material applicability is NOT_EVALUATED."
    parameters = (
        ("Fatigue safety factor, DFF (dimensionless)", "dff", 5, attributed),
        ("Creep safety factor (dimensionless)", "creep_safety_factor", 6, unsourced),
        ("T-N intercept, aD (cycles)", "tn_curve.intercept", 8, attributed),
        ("T-N slope, m (dimensionless)", "tn_curve.slope", 9, attributed),
        ("Mean-load knockdown coefficient (dimensionless)", "tn_curve.mean_load_knockdown", 10, unsourced),
        ("Creep reference life (years)", "creep.reference_life_years", 12, unsourced),
        ("Creep reference load ratio (dimensionless)", "creep.load_ratio_ref", 13, unsourced),
        ("Creep load-ratio exponent coefficient (dimensionless)", "creep.decades_per_load_ratio", 14, unsourced),
        ("Minimum allowable tension ratio (dimensionless)", "min_tension.min_ratio_allow", 16, unsourced),
    )
    parameter_table = table(
        ("Parameter and units", "Input value", "Input line", "Basis and qualification"),
        [[label, n("parameter." + key), f'<a href="../../../../examples/workflows/synthetic-rope-mooring-fatigue/input.yml#L{line}">input.yml:{line}</a>', basis]
         for label, key, line, basis in parameters],
        "Table F. Supplied model parameters; public configuration values are reproduced without source qualification.", "table-parameters",
    )
    # Reuse the existing page stylesheet and canonical tokens, with no network assets.
    exemplar = (ROOT / "docs/api/cfd/turbulent-flat-plate-verification.html").read_text(encoding="utf-8")
    page_style = re.search(r"<style>(.*?)</style>", exemplar, re.S).group(1)
    brand = (ROOT / "docs/api/_assets/brand.css").read_text(encoding="utf-8")
    brand = re.sub(r"/\*.*?\*/", "", brand, flags=re.S)
    styles = brand + page_style + """
svg{width:100%;height:auto;font:14px -apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif}
svg text{fill:var(--ink)} [data-point]{cursor:pointer} [data-point]:focus{outline:2px solid var(--accent)}
.plotbox{border:1px solid var(--line);border-radius:6px;margin:14px 0;padding:4px;background:#fff}
.table-scroll{overflow-x:auto} textarea{display:block;width:100%;min-height:100px;font:inherit}
button,select,input{font:inherit;margin:4px;padding:4px} .hs-doc th,.hs-doc .badge{font-weight:400}
"""
    body = f"""
<header><div class="kicker">Mooring integrity analysis</div>
<h1>Synthetic rope mooring fatigue analysis</h1>
<p class="sub">Module scope, calculation basis and public worked example</p>
<p>Draft for maintainer review</p></header>
<nav aria-label="Report sections"><a href="#summary">Summary</a> · <a href="#basis">Basis</a> ·
<a href="#results">Results</a> · <a href="#recommendations">Recommendations</a> ·
<a href="#references">References</a> · <a href="#appendix">Appendix</a> · <a href="#comments">Comments</a></nav>

<section id="summary"><h2>1. Executive summary</h2>
<p>The synthetic rope mooring fatigue module is an algebraic service-life screening tool for prescribed rope loads.
Analysis is performed to determine fatigue, creep-rupture and minimum-tension margins, and the key results are summarized below.</p>
{overall}
<ul><li>The public example is screened against its supplied material laws and acceptance limits.</li>
<li>Engineering acceptance is not established by these screening results.
<ul><li>Material-specific creep qualification, low-tension evidence and a site load basis are required before service use.</li>
<li>{action} The large fatigue lives are not service-life recommendations.</li></ul></li></ul>
<p>All numerical findings are calculated from the <a href="#record-example">public example</a> through the <a href="#record-module">module workflow</a>.
Detailed values are given in <a href="#table-results">Table B</a> and limits in <a href="#table-limits">Table D</a>.</p>
<div id="revision-history"><h3>Revision history</h3>
<table><thead><tr><th scope="col">Revision</th><th scope="col">Date</th><th scope="col">Change and cause</th></tr></thead>
<tbody><tr><td>Initial</td><td>2026-10-21</td><td>The screening workflow is documented to distinguish its calculated example from material qualification.</td></tr>
<tr><td>1</td><td>2026-10-22</td><td>The input basis is exposed and independent arithmetic checks are added; decisions and margin displays are corrected to follow the calculation.</td></tr></tbody></table></div>
<p>Legend: The supplied criterion is met. The supplied criterion is not met. Material qualification is not evaluated.</p>
{chart('creep', values)}
</section>

<section id="introduction"><h2>2. Introduction</h2>
<p>Synthetic rope service life is screened from specified tension ranges, cycle counts and load ratios.
The calculation is performed without a global mooring model or a licensed solver.</p>
<p>The main workflow covers tension-range fatigue, a supplied creep-life law and a minimum-tension ratio check for each synthetic span.
The least margin governs each line, and the least line margin governs the returned screening result.
An overall pass is returned only when every line meets every supplied criterion.</p>
<p>The documented public example is used as a numerical demonstration. It is not a published physical experiment or a qualified site design.
The module is classified as development maturity in the <a href="#record-registry">module registry</a>.</p>
</section>

<section id="basis"><h2>3. Design data and acceptance basis</h2>
<p>The design basis is supplied as a configuration mapping with consistent force and time units.
Loads and material laws are prescribed inputs; site conditions are not calculated.</p>
<h3>3.1 System and input basis</h3>
<p>The example design exposure is {n('design_life')} years. ASSUMED - to be confirmed:
the supplied cycle counts represent this complete design exposure, with no additional annualisation.
This interpretation is required because the module divides design life by the accumulated damage.</p>
<p>Water depth, span length, rope diameter, installation state, host motion, metocean weighting and measured tension histories are not supplied.
They are not invented. Geometry is shown only as a labelled load-path schematic in <a href="#mooring-schematic">the system figure</a>.
The physical input values and complete bin set are recorded in <a href="#table-inputs">Table A</a> and <a href="#table-bins">Table C</a>.</p>
<h3 id="criteria">3.2 Criteria and rationale</h3>
<p>The fatigue margin is L / (T × DFF). The creep margin is Lcreep / (T × creep safety factor).
The minimum-tension margin is Tmin / (MBL × minimum allowable tension ratio).
Every margin is required to reach unity; a life below its factored requirement or a tension below its prescribed minimum therefore fails the supplied criterion.
These definitions and the unity comparison are <a href="#record-module">implementation acceptance rules</a>, not an assertion of code compliance.</p>
<p>The public example attributes its polyester curve and fatigue factor to <a href="#ref-standard">Position mooring, October 2010, polyester fatigue provisions</a>.
That edition has not been opened for this analysis. The public configuration values are reproduced in Table F, including its attributed curve and factor; source qualification is NOT_EVALUATED.
The example's mean-load modifier, creep parameters and low-tension threshold are demonstration inputs; their material applicability is NOT_EVALUATED.
The complete unchanged configuration remains the input record identified in <a href="#record-example">Section 9.2</a>.</p>
{parameter_table}
</section>

<section id="method"><h2>4. Analysis methodology</h2>
<p>The documented workflow is called directly on the public configuration, with only the output directory redirected.
Prescribed loads are screened at the rope-span boundary shown below.</p>
{SCHEMATIC}
<h3>4.1 Fatigue theory and supplied material functions</h3>
<p>Linear fatigue accumulation is represented by Miner's rule:</p>
<div class="eq">D = Σ(nᵢ / Nᵢ); L = T / D.</div>
<p>D is accumulated fatigue damage, nᵢ is the cycle count in bin i, Nᵢ is the allowable count at that range,
T is the design exposure in years, and L is the extrapolated fatigue life in years.
The damage sum and its approximation limits are described in <a href="#ref-miner">the open institutional fatigue teaching note, May 2001, cumulative-damage section, equation (1)</a>.
The life expression follows by repeating the same exposure block: damage is proportional to elapsed exposure under that assumption.
This bookkeeping does not qualify a rope material curve.</p>
<p>The required fatigue input function maps tension range divided by breaking load to allowable cycles.
Its power-law intercept, slope and mean-load modifier are supplied in the public configuration, rather than selected by the module.
The implemented relation is N = aD × exp(−k × mean tension / MBL) × R<sup>−m</sup>, where R is tension range / MBL.
The public configuration attributes aD and m to the cited 2010 Table F3; k is an unsourced demonstration coefficient (Table F).
The public example disables that modifier; no mean-load fatigue sensitivity is demonstrated.
The claimed curve basis is identified in <a href="#ref-standard">the cited standard edition and clause titles</a>; qualification is NOT_EVALUATED because the primary text is not in hand.</p>
<p>The required creep input function maps mean tension divided by breaking load to creep life in years.
The supplied demonstration relation is Lcreep = Lref × 10<sup>(rref − mean tension / MBL) × c</sup>.
Lref, rref and c are the unsourced demonstration coefficients in Table F; no standard calibration is claimed.
No open material calibration is held for that input law. Its coefficients are required inputs and its physical qualification is NOT_EVALUATED;
the calculated creep life is conditional on that law. The minimum-tension threshold is likewise a required input, not a predicted axial-compression fatigue life.</p>
<h3>4.2 Interface, outputs and arithmetic checks</h3>
<p>The public workflow accepts a nonempty line list, material names, breaking load, mean and minimum tension,
positive tension ranges and cycle counts, explicit fatigue and creep models, and separate life safety factors.
Global model settings are required even when line overrides are provided.
Polyester and generic synthetic material labels are admitted. Nylon requires a line-specific fatigue curve;
high-modulus polyethylene requires line-specific fatigue and creep models. Chain and steel wire are rejected.</p>
<p>The same configuration mapping is mutated and returned. Its input settings are replaced by line summaries,
the governing line and mechanism, screening status and paths to the bin and line-summary comma-separated-value files.
A copy of the input mapping is therefore required when the original input record is needed after the call.
The bin file contains range, normalised range, count, allowable count and damage. The summary file contains calculated lives, margins and governing results.</p>
<p>Arithmetic is checked separately against hand-derived range and minimum-tension normalisation,
allowable cycles, both line damages and fatigue lives, fatigue margin, required creep life, governing creep margin and minimum-tension margins.
These are arithmetic anchors, not physical material tests. The rebuild test also checks every marked example value against a fresh call.</p>
</section>

<section id="results"><h2>5. Detailed results</h2>
<p>The supplied example is calculated by calling the module, and the line results are presented against their own supplied limits.
The plotted margins are dimensionless and increase with distance above the unity acceptance line.</p>
<h3>5.1 Fatigue and minimum tension</h3>
{''.join('<p>' + mechanism_statement(i, m, values) + '</p>' for i in range(len(LABELS)) for m in ('fatigue', 'compression'))}
<p>The supplied low-tension check does not calculate damage from repeated compression events.</p>
{chart('fatigue', values)}
{chart('compression', values)}
<h3>5.2 Creep and overall screening result</h3>
{''.join('<p>' + mechanism_statement(i, 'creep', values) + '</p>' for i in range(len(LABELS)))}
{overall}
<p>{action}
the basis is the separate life-limit definition in <a href="#criteria">Section 3.2</a> and the missing calibration recorded in <a href="#data-register">the data register</a>.</p>
<div id="chart-readout" class="callout" role="status" aria-live="polite">Select a chart marker to read its values and limit.</div>
<p>Full bin results, calculated life requirements and all returned numerical line fields are recorded in
<a href="#table-results">Table B</a>, <a href="#table-bins">Table C</a> and <a href="#table-limits">Table D</a>.</p>
</section>

<section id="findings"><h2>6. Key findings</h2>
<p>The screening outcome is controlled by the least of the separate mechanism margins.
Fatigue damage alone is insufficient to determine the overall result.</p>
{overall}
<p>The large calculated fatigue lives are extrapolations of the prescribed exposure and input curve, not recommended service lives.</p>
<p>Existing repository tests provide workflow status and curve-wiring checks.
No physical rope benchmark or source-qualified creep calibration is established by those assertions.
The evidence inventory and the checks performed for this report are separated in <a href="#qa">the quality-assurance appendix</a>.</p>
</section>

<section id="limits"><h2>7. Limitations</h2>
<p>The calculation is limited to prescribed scalar loads and supplied material functions.
Engineering acceptance remains not evaluated until material and exposure data are qualified.</p>
<h3>7.1 Limits of use</h3>
<p>Temperature is passed to the outputs without changing the fatigue or creep calculation.
There is no moisture, abrasion, ageing, bedding-in, bend-over-sheave, termination or load-sequence model.
The minimum-tension margin is a threshold screen, not an axial-compression damage model.
Normalisation by breaking load does not establish a characteristic-strength or residual-strength basis.</p>
<p>Positive-value guards do not establish a finite numerical domain or physical ordering of minimum, mean and peak tension.
Nonfinite inputs, duplicate line identifiers and extrapolation outside calibrated material ranges require checking before the call.
Extreme coefficients can cause overflow or underflow; a numerical return does not establish validity.
Output files are written sequentially, without transactional recovery, and repeated input stems can overwrite earlier results.</p>
<h3>7.2 Not covered</h3>
<p>The separate fatigue interpolation atlas, material database ingestion, stiffness and creep-elongation design utilities,
global station-keeping, host response, anchor design and metallic-line fatigue are not covered.
No dynamic solver, rainflow counting, tension-history generation, static equilibrium, recoil or geometric mesh is used in this workflow.</p>
</section>

<section id="recommendations"><h2>8. Recommendations</h2>
<p>Material qualification and exposure definition are required before these margins are used for service decisions.
The following actions are based on the failure and the missing-data entries recorded in this analysis.</p>
{overall}
<p>{action}
Reducing tension or changing capacity also changes global stiffness, offsets and anchor loads; those effects require a global mooring assessment outside this calculation.</p>
<p>A design-exposure tension histogram is required with explicit environmental weights and cycle-counting conventions.
The distinction between full tension range and half-range amplitude is recorded before evaluating the curve.
Minimum tension is checked against a material-qualified low-tension criterion, with monitoring tied to that criterion once established.
No operational trigger or inspection interval is invented while those data are absent.</p>
<p>The numerical interface is strengthened next by finite-value checks, physical load ordering, unique line identifiers,
explicit exposure units, provenance for material laws and non-overwriting output handling.
Additional reference cases then cover each mechanism at its acceptance boundary and outside its calibrated domain.
These actions follow the observed <a href="#record-module">implementation limits</a>, rather than a claim of standard compliance.</p>
</section>

<section id="references"><h2>9. References</h2>
<p>The calculation basis is separated into an open theory source, an unopened standard citation and repository evidence.
Repository locators are collected here so that the engineering narrative remains readable.</p>
<h3>9.1 Theory and standard basis</h3>
<p id="ref-miner"><a href="https://web.mit.edu/course/3/3.11/www/modules/fatigue.pdf">Institutional teaching note, Fatigue, May 2001 edition</a>,
section “Miner’s law for cumulative damage”, equation (1), printed pages 4–6.
The open copy was read on 2026-10-22. It supports linear damage bookkeeping and explains its limitations; it does not establish synthetic-rope coefficients.</p>
<p id="ref-standard">DNV-OS-E301, Position mooring, October 2010 edition, Table F3 (polyester tension-range fatigue) and F702 (polyester fatigue safety factor), as described by the public example.
This is the edition and provision basis attributed by the public example. Primary text was not opened;
clause titles and applicability require confirmation against the cited edition before design use. Table F reproduces the public inputs, without verifying the attributed standard values.</p>
<h3>9.2 Repository records and rebuild instructions</h3>
<ul>
<li id="record-example"><a href="../../../../examples/workflows/synthetic-rope-mooring-fatigue/README.md">Public example description</a> and
<a href="../../../../examples/workflows/synthetic-rope-mooring-fatigue/input.yml">complete public input configuration</a>.
Line order is preserved. Model coefficients and separate safety factors are read from this file without retyping them.</li>
<li id="record-module"><a href="../../../../src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py">Calculation implementation</a>:
public function <code>digitalmodel.synthetic_rope_mooring_fatigue.workflow.router(cfg)</code>.
<a href="../../../../src/digitalmodel/synthetic_rope_mooring_fatigue/docs/agent-pack.md">Existing module evidence inventory</a>.</li>
<li id="record-registry"><a href="../../../../docs/registry/module-routing.yaml">Module routing and maturity</a> and
<a href="../../../../docs/registry/workflows.yaml">documented workflow registration</a>.</li>
<li id="record-tests"><a href="../../../../tests/workflows/test_durable_workflows.py">Existing workflow tests</a>:
<code>test_workflow_registry[synthetic-rope-mooring-fatigue]</code> and
<code>test_synthetic_rope_tn_curve_matches_dnv_os_e301_table_f3</code>.</li>
<li id="record-report-test"><a href="../../../../tests/docs/test_synthetic_rope_mooring_fatigue_report.py">Report rebuild and arithmetic tests</a>.</li>
<li id="record-style">Family: riser report structure, adapted to synthetic-span service-life screening.
Layout exemplar: <a href="../../../api/cfd/turbulent-flat-plate-verification.html">existing engineering report page</a>;
colour and type tokens: <a href="../../../api/_assets/brand.css">canonical stylesheet</a>. Styles are inlined; the body uses <code>hs-doc</code>.</li>
</ul>
<p>The example is called with a copied configuration and a separate output directory:
<code>cfg = yaml.safe_load(input_text); cfg["synthetic_rope_mooring_fatigue"]["output_dir"] = scratch; result = router(cfg)</code>.</p>
<p>Rebuild from the repository root:
<code>PYTHONPATH=src uv run --no-project --with pyyaml python docs/reports/modules/syntheticropemooringfatigue/build_report.py --scratch &lt;scratch&gt;</code>.
The required scratch directory receives calculation files. The generated HTML and change record are written beside this script.
Numerical displays are rounded for presentation; the test compares them at that displayed precision.</p>
</section>

<section id="acronyms"><h2>10. Acronyms and supporting records</h2>
<p>Symbols and supporting records are provided for interpretation of the screening results.
The complete example values and data gaps are recorded in the appendices below.</p>
<p>MBL means minimum breaking load as named by the input interface. T-N means tension range versus allowable cycle count.
HMPE means high-modulus polyethylene. CSV means comma-separated values. QA means quality assurance.
The symbols D, nᵢ, Nᵢ, T and L are defined in Section 4.1. A margin is the calculated quantity divided by its required minimum;
unity is the acceptance boundary. NOT_EVALUATED means that no qualified engineering conclusion is established for the named item.</p>
<h3 id="appendix">Appendix A. Worked-example data and numerical results</h3>
<p>All quantities in these tables are read from the public configuration or returned by the module.
Derived requirements are the prescribed design exposure multiplied by its separate safety factor, or breaking load multiplied by the prescribed minimum ratio.
Scientific notation preserves small nonzero damage and large extrapolated lives.</p>
<div class="table-scroll">{input_table}{result_table}{bin_table}{requirements}</div>
<h3 id="data-register">Appendix B. Data register</h3>
<table><caption>Table E. Data held, missing data and their consequence.</caption>
<thead><tr><th scope="col">Item</th><th scope="col">Status and basis</th><th scope="col">Consequence or action</th><th scope="col">Source</th></tr></thead>
<tbody>
<tr><td>Loads and exposure</td><td>ASSUMED - to be confirmed: example values represent the complete design exposure.</td><td>Supply site-weighted counts and physical tension histories before service use.</td><td><a href="#record-example">Public example</a></td></tr>
<tr><td>Polyester fatigue curve and factor</td><td>NOT_EVALUATED: primary standard text is not in hand; Table F reproduces the attributed public inputs.</td><td>Check the cited edition and material/strength applicability.</td><td><a href="#ref-standard">Standard citation</a></td></tr>
<tr><td>Creep safety factor</td><td>Unsourced demonstration value in Table F; the required life is design exposure multiplied by this factor.</td><td>Qualify this separate factor before using its factored creep-life requirement for service decisions.</td><td><a href="#record-example">Public input line 6</a></td></tr>
<tr><td>Edition conflict</td><td>The example attributes October 2010; the parametric provenance record pins 2018-07.</td><td>Resolve edition and provision applicability against primary text; neither attribution establishes qualification.</td><td><a href="../../../../src/digitalmodel/parametric/refresh.py#L35">Parametric provenance record, line 35</a></td></tr>
<tr><td>Creep law and low-tension threshold</td><td>ASSUMED - to be confirmed: supplied demonstration inputs; material qualification is NOT_EVALUATED.</td><td>Obtain supplier test evidence at the service load, temperature and environment.</td><td><a href="#record-example">Public example</a></td></tr>
<tr><td>Geometry, host and metocean</td><td>Not supplied; no assumption is used to produce a geometric or dynamic result.</td><td>Global mooring loads and boundary motions require a separate assessment.</td><td><a href="#record-module">Input interface</a></td></tr>
<tr><td>Temperature and environment</td><td>Not supplied in the example; temperature does not modify the implemented laws.</td><td>Restrict interpretation to arithmetic until material applicability is established.</td><td><a href="#record-module">Implementation</a></td></tr>
<tr><td>Physical benchmark</td><td>No physical rope experiment is present in the reviewed workflow evidence.</td><td>Arithmetic agreement does not establish a service envelope.</td><td><a href="#record-tests">Existing tests</a></td></tr>
</tbody></table>
<h3 id="qa">Appendix C. Calculation and quality-assurance evidence</h3>
<p>The existing workflow registry test asserts the two line statuses, overall creep failure, positive bin outputs and decreasing allowable cycles with increasing range.
The existing curve-wiring test compares calculated allowable cycles with the same input power law; it is a regression assertion, not an independently sourced physical benchmark.
Both named workflow nodes in Section 9.2 passed with their application dependencies installed; they run in the <code>tests-workflows</code> CI shard.
The two rope-specific atlas nodes, <code>test_synthetic_rope_fatigue_atlas_is_log_log_exact</code> and <code>test_fatigue_bins_handler_shared_by_both_ropes</code>, also passed.
No solver integration is imported by this report.</p>
<p>The report test suite checks the full rebuild, every marked example value, hand-derived arithmetic anchors,
the section and reference structure, offline assets, interactive chart bindings, public wording and explicit UTF-8 text input/output with line-feed output.
The report tests are run with the interpreter's UTF-8 override unset.
The mathematical anchors are range divided by breaking load, minimum tension divided by breaking load,
minimum tension divided by required minimum tension, independent power-law cycle counts and damage sums, exposure divided by damage,
the reciprocal factored damage, the factored creep exposure and the square-root form of the second line's prescribed creep calculation divided by its requirement.
The test source records each arithmetic derivation.</p>
<p>Pre-flight record: system scope and boundary inputs are named; units and exposure are stated; acceptance criteria and their qualification status are stated;
all plotted limits are drawn; numerical example values come from the calculation; missing data and the action for the failing result are recorded;
all local reference targets are checked; and the built page is scanned for public wording.
Material qualification remains NOT_EVALUATED.</p>
<h3 id="changes">Appendix D. Engineering changes</h3>
<details open><summary>Changes since the preceding issue</summary><div id="changes-panel"></div></details>
<h3 id="comments">Appendix E. Review comments</h3>
<p>Select the folder containing this report to load and save <code>report.comments.json</code> beside it.
Comments are held in memory until saved. If folder access is unavailable, export the file, place it beside the report and import it when reopening.
No comments are transmitted over a network.</p>
<button id="comment-folder" type="button">Select report folder</button>
<form id="comment-form"><label for="comment-section">Section</label><select id="comment-section">
{''.join(f'<option>{label}</option>' for label in ('Summary', 'Introduction', 'Design data', 'Methodology', 'Results', 'Findings', 'Limitations', 'Recommendations', 'References', 'Appendices'))}
</select><label for="comment-text">Comment</label><textarea id="comment-text" required></textarea>
<button type="submit">Add comment</button></form><ol id="comment-list"></ol>
<button id="comment-save" type="button">Save or export comments</button>
<label for="comment-import">Import comments file</label><input id="comment-import" type="file" accept=".json,application/json">
<p id="comment-status" role="status" aria-live="polite">No comments have been saved.</p>
</section>
<footer>Draft engineering analysis. Results are conditional on the prescribed exposure and supplied material laws.</footer>
"""
    record = json.dumps({key: display(value, key) for key, value in values.items()}, ensure_ascii=False, sort_keys=True)
    changes = json.dumps(CHANGES, ensure_ascii=False, sort_keys=True)
    result = (
        '<!DOCTYPE html>\n<html lang="en" data-theme="light"><head><meta charset="utf-8">\n'
        '<meta name="viewport" content="width=device-width, initial-scale=1">\n'
        '<title>Synthetic rope mooring fatigue analysis</title>\n'
        f'<style>{styles}</style>\n</head><body class="hs-doc"><main class="wrap">\n{body}\n'
        f'</main><script id="numeric-record" type="application/json">{record}</script>\n'
        f'<script id="change-record" type="application/json">{changes}</script>\n'
        f'<script>{INTERACTION}</script>\n</body></html>\n'
    )
    result = "\n".join(line.rstrip() for line in result.splitlines()) + "\n"
    return result, copy.deepcopy(CHANGES)


def main() -> None:
    """Write the report and engineering change record with portable text I/O."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--scratch", type=Path, required=True)
    parser.add_argument("--output-dir", type=Path, default=HERE)
    args = parser.parse_args()
    scratch = args.scratch.resolve()
    if scratch == ROOT or ROOT in scratch.parents:
        parser.error("scratch must be outside the repository")
    report, changes = build(scratch)
    args.output_dir.mkdir(parents=True, exist_ok=True)
    (args.output_dir / "report.html").write_text(report, encoding="utf-8", newline="\n")
    (args.output_dir / "changes.json").write_text(
        json.dumps(changes, ensure_ascii=False, indent=2) + "\n", encoding="utf-8", newline="\n"
    )
    print("Report and engineering change record rebuilt.")


if __name__ == "__main__":
    main()
