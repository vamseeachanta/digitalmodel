# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Build the self-contained corrosion report from calls to the public module.

Run with the source tree on PYTHONPATH. Optional --output-dir keeps rebuilds
outside the repository. No solver or network access is used by this script.
"""

from __future__ import annotations

import argparse
import json
import math
import re
from dataclasses import asdict
from html import escape
from pathlib import Path

from digitalmodel.corrosion import (
    ANODIC_INDEX_V,
    ENVIRONMENTS,
    compatibility_matrix,
    screen_couple,
)
from digitalmodel.corrosion import galvanic_screening as screening

ROOT = Path(__file__).resolve().parents[2]
DESTINATION = ROOT / "docs/reports/modules/corrosion"
REVISION = "1"
CHANGES = {
    "revision": REVISION,
    "history": [{
        "revision": REVISION,
        "date": "2026-10-22",
        "change": "Report dispositions follow returned categories and material roles; figures, material-group wording and deterministic output are corrected.",
    }, {
        "revision": "0",
        "date": "2026-10-22",
        "change": "Galvanic screening results and evidence gaps are documented to separate software categories from engineering acceptance.",
    }],
    "changes": [
        "Potential differences and environment limits are reported for the public zinc–steel regression case.",
        "Default numerical criteria are separated from independently supported physical principles.",
        "Dispositions and material roles follow calculation records; chart scales and the conductive-film sketch are corrected.",
        "A protection category for a sacrificial coating is distinguished from rejection of that coating.",
    ],
}
ENV_LABELS = {
    "harsh_marine": "Harsh marine",
    "normal_industrial": "Normal industrial",
    "controlled_indoor": "Controlled indoor",
}
STATUS = {
    "OK": "The difference is within the software allowable.",
    "MARGINAL": "The difference exceeds the software allowable within its marginal band.",
    "PROTECT": "The difference exceeds the software marginal band; protection is requested.",
}
SECTIONS = [
    "Executive summary", "Introduction", "Design data and basis",
    "Analysis methodology", "Detailed results", "Key findings",
    "Limits of use", "Recommendations", "References", "Appendices and symbols",
]


def calculations() -> dict:
    """Call the module and compare matrix cells with the public pair results."""
    cases = []
    symmetric = diagonal = matches = True
    metals = list(ANODIC_INDEX_V)
    for environment in ENVIRONMENTS:
        result = screen_couple("zinc", "carbon_steel", environment)
        case = asdict(result)
        case["excess_v"] = round(result.delta_v - result.allowable_delta_v, 6)
        case["marginal_upper_v"] = round(result.allowable_delta_v + screening.MARGINAL_BAND_V, 6)
        cases.append(case)
        matrix = compatibility_matrix(environment)
        symmetric &= matrix.equals(matrix.T)
        diagonal &= all(matrix.loc[metal, metal] == "OK" for metal in metals)
        for metal_a in metals:
            for metal_b in metals:
                pair = screen_couple(metal_a, metal_b, environment)
                matches &= matrix.loc[metal_a, metal_b] == pair.verdict
    return {
        "indices": {metal: ANODIC_INDEX_V[metal] for metal in ("zinc", "carbon_steel")},
        "cases": cases,
        "marginal_band_v": screening.MARGINAL_BAND_V,
        "metals_count": len(ANODIC_INDEX_V),
        "environments_count": len(ENVIRONMENTS),
        "matrix_symmetric": bool(symmetric),
        "matrix_diagonal_ok": bool(diagonal),
        "matrix_matches_pairs": bool(matches),
    }


def display(value: float) -> str:
    """Use engineering precision for the potential-difference screen."""
    return str(value) if isinstance(value, int) else f"{value:.2f}".replace("-", "−")


def number(data: dict, key: str) -> str:
    """Bind visible values to their calculation record."""
    value = data
    for part in key.split("."):
        value = value[int(part)] if isinstance(value, list) else value[part]
    return f'<span data-number="{key}">{display(value)}</span>'


def table(headers: list[str], rows: list[list[str]], caption: str) -> str:
    """Render actual table columns with a caption below the data."""
    head = "".join(f'<th scope="col" style="white-space:normal;text-align:left">{label}</th>' for label in headers)
    body = "".join('<tr>' + ''.join(f'<td style="white-space:normal;text-align:left">{cell}</td>' for cell in row) + '</tr>' for row in rows)
    return f'<div class="tbl-wrap"><table><caption style="caption-side:bottom;text-align:left">{caption}</caption><thead><tr>{head}</tr></thead><tbody>{body}</tbody></table></div>'


def house_style() -> str:
    """Inline the existing report stylesheet without changing its design."""
    template = (ROOT / "src/digitalmodel/reporting/assets/calc_report_format.html").read_text()
    return re.search(r'<style>.*?</style>', template, re.S).group(0)


def heading(index: int) -> str:
    """Use the established report heading layout."""
    return f'<div class="l1head"><h2 id="section-{index}">{index}. {SECTIONS[index - 1]}</h2></div>'


def chart(data: dict) -> str:
    """Plot potential difference and both software limits against environment."""
    x_values = [150 + i * 235 for i in range(len(data["cases"]))]
    maximum = math.ceil(max(
        max(case["delta_v"], case["marginal_upper_v"])
        for case in data["cases"]
    ) * 10) / 10
    maximum = max(maximum, 0.1)

    def y(value):
        return 300 - value / maximum * 240

    series = []
    for field, label, color in (
        ("delta_v", "Potential difference", "var(--ro)"),
        ("allowable_delta_v", "Software allowable", "var(--cfd)"),
        ("marginal_upper_v", "Upper marginal boundary", "var(--proj)"),
    ):
        series.append(f'<g data-limit="{field}">')
        for x, case in zip(x_values, data["cases"]):
            text = (f'Zinc–steel case; component: zinc coating and steel substrate; '
                    f'environment: {ENV_LABELS[case["environment"]]}; '
                    f'demand: {display(case["delta_v"])} V; '
                    f'limit: {display(case["allowable_delta_v"])} V; '
                    f'upper marginal boundary: {display(case["marginal_upper_v"])} V; '
                    f'{label}: {display(case[field])} V.')
            series.append(f'<circle tabindex="0" role="img" aria-label="{escape(text)}" data-chart-note="{escape(text)}" cx="{x}" cy="{y(case[field]):.2f}" r="7" fill="{color}"><title>{escape(text)}</title></circle>')
        series.append('</g>')
    ticks = ''.join(
        f'<g data-y-tick="{i / 10:.1f}"><path d="M75 {y(i / 10):.2f} H700" '
        f'stroke="currentColor" opacity="0.2"/>'
        f'<text x="68" y="{y(i / 10) + 5:.2f}" text-anchor="end" '
        f'fill="currentColor">{i / 10:.1f}</text></g>'
        for i in range(round(maximum * 10) + 1)
    )
    labels = ''.join(f'<text x="{x}" y="330" text-anchor="middle" fill="currentColor">{ENV_LABELS[case["environment"]]}</text>' for x, case in zip(x_values, data["cases"]))
    return f'''<figure id="environment-chart"><svg viewBox="0 0 770 390" role="img" aria-labelledby="chart-title chart-description" style="width:100%">
<title id="chart-title">Zinc–steel potential difference by environment</title>
<desc id="chart-description">Separate markers show potential difference, software allowable and upper marginal boundary; environment classes are discrete. Focus or hover a point for values in volts.</desc>
<path d="M80 40 V300 H700" fill="none" stroke="currentColor"/>
<text x="25" y="180" transform="rotate(-90 25 180)" text-anchor="middle" fill="currentColor">Potential difference (V)</text>
<text x="385" y="365" text-anchor="middle" fill="currentColor">Environment class</text>
{ticks}{''.join(series)}{labels}</svg>
<figcaption>Figure B. Potential difference and the repository screening boundaries. Blue markers: demand. Green markers: allowable. Orange markers: upper marginal boundary. Markers are not joined between environment classes. Values are also tabulated below.</figcaption>
<p id="chart-readout" aria-live="polite">Focus or hover a plotted point to read the case, component, demand, limit and units.</p></figure>'''


INTERACTION = r'''
(() => {
  'use strict';
  const data = JSON.parse(document.getElementById('calculation-data').textContent);
  const changes = JSON.parse(document.getElementById('change-data').textContent);
  const list = document.getElementById('changes-list');
  for (const text of changes.changes) {
    const item = document.createElement('li'); item.textContent = text; list.appendChild(item);
  }
  for (const point of document.querySelectorAll('[data-chart-note]')) {
    const show = () => {document.getElementById('chart-readout').textContent = point.dataset.chartNote;};
    point.addEventListener('focus', show); point.addEventListener('pointerenter', show);
  }
  const message = document.getElementById('comment-message');
  let comments = [];
  let handle = null;
  const evidence = JSON.stringify(data);
  const show = () => {document.getElementById('comments-preview').textContent = JSON.stringify(comments, null, 2);};
  document.getElementById('comment-form').addEventListener('submit', event => {
    event.preventDefault();
    const form = event.target;
    comments.push({section: form.elements.section.value, text: form.elements.comment.value});
    form.elements.comment.value = ''; show();
    message.textContent = 'Comment recorded in this page. Save comments to retain it.';
  });
  document.getElementById('load-comments').addEventListener('change', async event => {
    try {
      const file = event.target.files[0]; if (!file) return;
      const incoming = JSON.parse(await file.text());
      if (incoming.report !== 'Corrosion module analysis' || incoming.revision !== changes.revision ||
          incoming.evidence !== evidence || !Array.isArray(incoming.comments) ||
          incoming.comments.some(c => typeof c.text !== 'string' ||
            !Array.from(document.getElementById('comment-section').options).some(o => o.value === c.section)))
        throw new Error('The comments belong to different report evidence or have invalid fields.');
      comments = comments.concat(incoming.comments); show();
      message.textContent = 'Matching comments loaded. Save to retain the combined comments.';
    } catch (error) {message.textContent = error.message;}
    event.target.value = '';
  });
  document.getElementById('save-comments').addEventListener('click', async () => {
    const payload = JSON.stringify({report: 'Corrosion module analysis', revision: changes.revision,
      evidence, comments}, null, 2) + '\n';
    try {
      if (typeof window.showSaveFilePicker === 'function') {
        if (!handle) handle = await window.showSaveFilePicker({suggestedName: 'report.comments.json',
          types: [{description: 'Report comments', accept: {'application/json': ['.json']}}]});
        const writable = await handle.createWritable(); await writable.write(payload); await writable.close();
        if (await (await handle.getFile()).text() !== payload) throw new Error('Saved comments differ from the page.');
        message.textContent = 'Comments saved and read back. Keep this file beside the report.';
      } else {
        const url = URL.createObjectURL(new Blob([payload], {type: 'application/json'}));
        const anchor = document.createElement('a'); anchor.href = url; anchor.download = 'report.comments.json';
        anchor.click(); setTimeout(() => URL.revokeObjectURL(url), 1000);
        message.textContent = 'Comments download requested. Move the file beside the report and load it to check the saved copy.';
      }
    } catch (error) {message.textContent = error.message;}
  });
})();
'''


def render() -> str:
    """Write the complete technical narrative with the module in context."""
    data = calculations()
    n = lambda key: number(data, key)
    harsh, _, indoor = data["cases"]
    anodic = escape(harsh["anodic"].replace("_", " ").capitalize())
    cathodic = escape(harsh["cathodic"].replace("_", " "))
    rows = []
    for i, case in enumerate(data["cases"]):
        rows.append([
            ENV_LABELS[case["environment"]], n(f"cases.{i}.delta_v"),
            n(f"cases.{i}.allowable_delta_v"), n(f"cases.{i}.excess_v"),
            STATUS[case["verdict"]], '<a href="#repository-basis">Repository calculation basis</a>',
        ])
    results_table = table(
        ["Environment", "Difference (V)", "Allowable (V)", "Difference minus allowable (V)", "Screening result", "Source"],
        rows, "Table A. Zinc–steel screen. A negative difference minus allowable is below the software limit; no physical safety margin is established.",
    )
    revisions = table(
        ["Revision", "Date", "What changed and why"],
        [[row["revision"], row["date"], row["change"]] for row in CHANGES["history"]],
        "Report issue history; newest issue first.",
    )
    checks = [
        ("Matrix symmetry", data["matrix_symmetric"]),
        ("Same-material diagonal within the software allowable", data["matrix_diagonal_ok"]),
        ("Matrix cells match public pair calls", data["matrix_matches_pairs"]),
    ]
    if not all(passed for _, passed in checks):
        raise ValueError("Matrix evidence differs from the module")
    check_rows = [[label, "The evidence agrees.", '<a href="#repository-basis">Repository calculation basis</a>'] for label, _ in checks]
    nav = ''.join(f'<li><a href="#section-{i}">{title}</a></li>' for i, title in enumerate(SECTIONS, 1))
    options = ''.join(f'<option value="section-{i}">{title}</option>' for i, title in enumerate(SECTIONS, 1))
    body = f'''
<header class="hero"><div class="wrap"><h1>Corrosion module analysis</h1>
<p class="lede">Galvanic compatibility screening: calculated categories and evidence limits</p>
<p>Draft for review</p></div></header>
<div class="shell"><nav class="toc" aria-label="Contents"><ol>{nav}</ol></nav>
<main class="main">
<section>{heading(1)}
<p>The corrosion module is a dissimilar-metal compatibility screening tool. Analysis is performed to determine the potential difference and protection category of a zinc–steel couple; key results are summarized below.</p>
<ul><li>Screen material pairs before detailed corrosion assessment.
<ul><li>The zinc–steel difference is {n('cases.0.delta_v')} V against the harsh-marine software allowable of {n('cases.0.allowable_delta_v')} V. {STATUS[harsh['verdict']]}</li></ul></li>
<li>Retain the distinction between software screening and engineering acceptance.
<ul><li>Independent applicability of the numerical defaults is NOT_EVALUATED. No corrosion rate, coating life or structural acceptance is established.</li></ul></li></ul>
<p>Numerical values are taken from the <a href="#repository-basis">repository calculation basis</a>. The result for intentional zinc protection does not establish rejection of a galvanized component.</p>
<div id="revision-history"><h3>Revision history</h3>{revisions}</div>
</section>
<section>{heading(2)}
<p>Compatibility is screened from material-group indices and an environment category. The main workflow is evaluated through the public pair-screening interface and its matrix companion.</p>
<h3>Purpose and scope</h3>
<p>The report is used to identify pairs requiring engineering attention, reproduce the public zinc–steel example and locate the evidence needed before design use. Repository tests and the published explorer provide software evidence; an independent exposure experiment is not supplied by those records.</p>
<h3>Not covered</h3>
<p>Corrosion-rate models in the adjacent cathodic protection package, anode sizing, impressed-current design, stray-current assessment, inspection-based remaining life, pitting and stress-corrosion cracking are outside this module and this analysis.</p>
</section>
<section>{heading(3)}
<p>The public regression example is represented as a zinc coating electrically connected to a steel substrate. The physical arrangement is used to explain the screening result; dimensions and exposure history are absent from the example.</p>
<h3>Physical arrangement and boundary conditions</h3>
<figure><svg viewBox="0 0 760 300" role="img" aria-labelledby="joint-title joint-description" style="width:100%">
<title id="joint-title">Section through a wetted zinc-coated steel surface</title>
<desc id="joint-description">A surface defect exposes steel through a zinc coating. A conductive wet film bridges zinc and steel; the substrate provides electrical continuity. No dimensions or mesh are specified.</desc>
<path id="wet-film" d="M60 55 H700 V110 H420 V125 H320 V110 H60 Z" fill="var(--cfd-soft)" stroke="var(--cfd)"/>
<rect x="60" y="110" width="260" height="30" fill="var(--surface-2)" stroke="currentColor"/>
<rect x="420" y="110" width="280" height="30" fill="var(--surface-2)" stroke="currentColor"/>
<path d="M60 140 H320 V125 H420 V140 H700 V235 H60 Z" fill="var(--ro-soft)" stroke="currentColor"/>
<text x="75" y="87" fill="currentColor">Conductive wet film: ionic path</text>
<text x="75" y="132" fill="currentColor">Zinc coating</text>
<text x="450" y="132" fill="currentColor">Zinc coating</text>
<text x="75" y="210" fill="currentColor">Steel substrate: electrical continuity</text>
<path d="M245 175 H480 M470 169 L480 175 L470 181" fill="none" stroke="currentColor"/>
<text x="250" y="165" fill="currentColor">Electron path towards exposed steel</text>
<path d="M370 30 V120" stroke="currentColor"/>
<text x="365" y="23" text-anchor="middle" fill="currentColor">Surface defect exposes steel</text>
<text x="60" y="275" fill="currentColor">Not to scale. Coating thickness, exposed areas and wet-film chemistry are unspecified.</text>
</svg><figcaption>Figure A. Zinc–steel coating cross-section with the assumed conductive paths. The algebraic screen has no mesh or field boundary solution.</figcaption></figure>
<h3>Inputs and acceptance basis</h3>
<p>Material keys, environment key, an optional environment-to-voltage mapping and an optional marginal-band width are accepted. The shipped catalogue contains {n('metals_count')} material groups and {n('environments_count')} environment classes, counted directly from the <a href="#repository-basis">repository tables</a>.</p>
<p>The example uses the zinc wrought / die-cast / plated group, as named by the implementation; it does not represent a separately qualified hot-dip galvanizing group. The input indices are {n('indices.zinc')} V for zinc and {n('indices.carbon_steel')} V for carbon steel, relative to the module's gold reference. The marginal band is {n('marginal_band_v')} V. These are implementation data, not independently established electrochemical properties or approved criteria. Their external edition and table provenance are NOT_EVALUATED.</p>
<p>ASSUMED - to be confirmed: the wetted coating and substrate remain electrically connected. Area ratio, alloy condition, water chemistry, temperature, coating thickness and design life are required for a physical corrosion assessment and remain NOT_EVALUATED here. No numerical substitute is introduced.</p>
<p>The software limit is retained to reproduce existing behaviour, not to set a design acceptance criterion. A qualified environment limit is required before an engineering disposition is made. The <a href="#faa-basis">public physical guidance</a> supports considering electrical contact, electrolyte and relative surface areas.</p>
</section>
<section>{heading(4)}
<p>An absolute index difference is calculated and compared with an environment-dependent allowable. The returned category is symmetric in material order; the anodic designation follows the larger index.</p>
<h3>Equations and category definitions</h3>
<div class="eq"><span class="m">ΔV = | I<sub>a</sub> − I<sub>b</sub> |</span></div>
<p>ΔV is potential difference in volts; I<sub>a</sub> and I<sub>b</sub> are the material-group anodic indices in volts. L is the environment allowable in volts and B is the marginal-band width in volts. The index definition and category equations are the <a href="#repository-basis">repository screening algorithm</a>; they are not a mixed-potential or current-balance solution.</p>
<ul><li>The difference is within the software allowable when ΔV ≤ L; the returned category is “OK”.</li>
<li>The difference exceeds the software allowable within its marginal band when L &lt; ΔV ≤ L + B; the returned category is “MARGINAL”.</li>
<li>The difference exceeds the software marginal band when ΔV &gt; L + B; protection is requested and the returned category is “PROTECT”.</li></ul>
<p>A floating-point comparison allowance and output rounding are applied by the implementation. They are numerical conventions, not physical uncertainty bounds; their exact definitions are recorded in the repository algorithm reference.</p>
<h3>Outputs and use sequence</h3>
<p>A result record contains both material keys, environment key, potential difference, allowable difference, category, anodic and cathodic members, and explanatory notes. A square table of categories is returned by the matrix interface. The fastener and coating notes are qualitative and are not an area-ratio calculation.</p>
<p>The documented material keys are selected, an environment basis is qualified, the pair is screened and the returned difference is compared with the returned limit. Unknown material or environment keys raise an error. Custom threshold mappings are supported; numerical input validation for negative or non-finite custom values is not established by the existing tests.</p>
</section>
<section>{heading(5)}
<p>The zinc–steel public regression case is calculated by calling the module. Environment variation is then calculated with the same material pair; no measured corrosion behaviour is inferred.</p>
<article id="worked-example"><h3>Worked example: zinc coating and carbon steel</h3>
<p>The source is the <a href="#public-example">public regression example</a>. The supplied indices give |{n('indices.zinc')} − {n('indices.carbon_steel')}| = {n('cases.0.delta_v')} V. The returned harsh-marine allowable is {n('cases.0.allowable_delta_v')} V and the marginal-band width is {n('marginal_band_v')} V.</p>
<p>The difference minus allowable is {n('cases.0.excess_v')} V; the upper marginal boundary is {n('cases.0.marginal_upper_v')} V. {STATUS[harsh['verdict']]} {anodic} is returned as anodic and {cathodic} as cathodic.</p>
<p>The returned request is a generic dissimilar-metal category. For an intentional sacrificial zinc coating, zinc consumption is part of the protection mechanism; the example uses the wrought / die-cast / plated material group and does not qualify hot-dip galvanizing; coating adequacy is NOT_EVALUATED by this screen. For an unintended conductive joint, electrical isolation is the recommended next design action, supported by the <a href="#faa-basis">public physical guidance</a>.</p></article>
{chart(data)}
{results_table}
<p>The potential difference remains fixed while the environment limit changes. Controlled indoor: {STATUS[indoor['verdict']]} This category establishes no coating life or field performance.</p>
<h3>Existing validation evidence</h3>
<p>The existing tests exercise index anchors, thresholds, material-order symmetry, identical-metal cases, boundary examples, ordering with potential difference, anodic designation, unknown keys, custom thresholds and matrix consistency. Anchor tests repeat repository values and are not independent evidence for their physical accuracy.</p>
{table(['Evidence comparison', 'Result', 'Source'], check_rows, 'Table B. Recomputed software consistency evidence across the complete current material catalogue and environment set.')}
<p>The published explorer is a second presentation of the same module calculations. Agreement with it is regression evidence, not independent validation. No measured-rate dataset or independently qualified physical benchmark is supplied by these cited screening records.</p>
</section>
<section>{heading(6)}
<p>Harsh marine: {STATUS[harsh['verdict']]} The anodic direction is reproduced across material order and matrix presentation.</p>
<p>The example establishes deterministic software behaviour against repository data. External provenance for numerical acceptance criteria, coating adequacy and physical rate prediction remain NOT_EVALUATED. The harsher environment label changes the threshold rather than predicting the effects of salinity, wetting time or temperature.</p>
</section>
<section>{heading(7)}
<p>Use is restricted to material-pair screening with explicit qualification of the inputs and criteria. No structural, coating-life or maintenance interval decision is established.</p>
<p>Passivation, local chemistry, polarization, electrical resistance, current distribution and exposed area are not model inputs. The <a href="#faa-basis">public physical guidance</a> notes that service conditions can alter potential differences and even relative ordering. Material-group indices therefore do not establish a site-specific galvanic driving voltage.</p>
<p>The source description attributes the defaults to a dissimilar-metals standard without an edition or clause. That attribution is retained as an unresolved source claim; no numerical value is asserted as a requirement of that standard. The marginal category is a software convention and supplies no physical safe-use band.</p>
<p>Changing the optional marginal width does not update the fixed-width wording in the module's marginal note. Use the returned category and numeric fields when custom settings are applied, and obtain a corrected explanatory note before relying on that text.</p>
</section>
<section>{heading(8)}
<p>Engineering acceptance is deferred until the material condition, exposure and governing criterion are established. Actions are assigned to the evidence gaps and to the calculated protection request.</p>
<ul><li>For an unintended zinc–steel conductive joint, introduce electrical isolation and confirm separation of the conductive path at detailed design. This action addresses the protection request and is based on elimination of electrical contact in the <a href="#faa-basis">public physical guidance</a>.</li>
<li>For an intentional sacrificial zinc coating, retain the coating intent and assess coating thickness, exposed steel area, environment and depletion before defining an inspection interval. This recommendation follows the missing physical inputs and the area effect described by the <a href="#faa-basis">public physical guidance</a>; no unsupported service date or depletion rate is assigned.</li>
<li>Before adopting a numerical default as an acceptance criterion, obtain its public edition and table or replace it with an approved required input. This action resolves the missing external provenance recorded in the <a href="#data-register">data register</a>.</li>
<li>As the first extension, add sourced criterion metadata, finite non-negative custom-input checks and notes derived from the selected marginal width. Preserve the existing public examples as regression checks and add tests at the selected boundaries. The scope follows the interface limitations and existing <a href="#repository-basis">repository evidence</a>.</li></ul>
</section>
<section>{heading(9)}
<p>The physical guidance and repository records are separated by their evidential role. Numerical defaults are cited to the implementation snapshot accompanying this report; their external provenance remains unresolved.</p>
<h3>9.1 Public physical guidance</h3>
<p id="faa-basis"><a href="https://www.faa.gov/documentLibrary/media/Advisory_Circular/AC_43-4B.pdf">Federal Aviation Administration, Advisory Circular 43-4B, Corrosion Control for Aircraft, issued 2018</a>: “Background”, paragraphs 2.2.1–2.2.2; “Galvanic Corrosion”, paragraph 2.5.3; “Corrosion Control in Design”, paragraph 4.12.2.5. The public file was read successfully on 2026-10-22. It supports the qualitative mechanism and recommendations, not the numerical index catalogue or environment limits in this report.</p>
<p>The implementation cites MIL-STD-889, Dissimilar Metals, without identifying an edition or table. A supporting edition and table were not read; the numerical attribution is NOT_EVALUATED and is not used as evidence of compliance.</p>
<h3>9.2 Repository records and public example</h3>
<ul><li id="repository-basis">Implementation snapshot accompanying this report: <code>src/digitalmodel/corrosion/galvanic_screening.py</code>, tables <code>ANODIC_INDEX_V</code> and <code>ENVIRONMENTS</code>, constant <code>MARGINAL_BAND_V</code>, functions <code>screen_couple</code>, <code>verdict_for_delta</code> and <code>compatibility_matrix</code>. The category comparison adds a numerical allowance of <code>1e−12</code> V and returns the difference rounded to <code>6</code> decimal places; these are implementation conventions, not engineering coefficients.</li>
<li>Public entry points: <code>src/digitalmodel/corrosion/__init__.py</code> exports the screening function, matrix function and result class. The function docstrings specify input keys and returned fields. The example is reproduced with this call:</li>
</ul><pre><code>from digitalmodel.corrosion import screen_couple
result = screen_couple("zinc", "carbon_steel", "harsh_marine")
print(result.delta_v, result.allowable_delta_v, result.verdict)</code></pre><ul>
<li id="public-example">Public regression example: <code>tests/corrosion/test_galvanic_screening.py</code>, <code>TestScreenCouple.test_zinc_carbon_steel_harsh_marine</code>. The same file supplies boundary and matrix evidence.</li>
<li>Published example presentation: <code>docs/api/corrosion/galvanic-compatibility-explorer.html</code> and its adjacent JSON data; generator <code>scripts/corrosion/build_galvanic_explorer.py</code>. These records reuse the same model and do not constitute an independent benchmark.</li>
<li>House report layout: <code>src/digitalmodel/reporting/assets/calc_report_format.html</code>, also rendered in <code>docs/domains/cathodic_protection/reports/cases/S01/report.html</code>. Its stylesheet is inlined without redesign; the requested report body class is applied.</li>
<li>Report reproduction: <code>scripts/corrosion/build_module_report.py</code>; rebuild and numerical binding checks: <code>tests/corrosion/test_module_report.py</code>. Run with the source tree on the Python import path.</li></ul>
</section>
<section>{heading(10)}
<p>The data register distinguishes assumed physical conditions from missing evidence. Calculation checks establish numerical consistency within the stated software basis.</p>
<h3 id="data-register">10.1 Data register</h3>
{table(['Item', 'Basis and status', 'Required resolution', 'Source'], [
    ['Wet film and electrical contact', 'ASSUMED - to be confirmed', 'Confirm the actual conductive paths before applying the pair screen.', 'Figure A and public physical guidance'],
    ['Material indices and default thresholds', 'Repository values; external provenance is NOT_EVALUATED.', 'Provide an edition and table or approved replacement inputs.', 'Repository calculation basis'],
    ['Marginal-band width', 'Repository convention; physical qualification is NOT_EVALUATED.', 'Qualify a decision rule before engineering acceptance.', 'Repository calculation basis'],
    ['Alloy condition, exposed areas and chemistry', 'NOT_EVALUATED; required physical inputs.', 'Supply material and exposure records for a physical assessment.', 'Model input limitations'],
    ['Thickness, design life and inspection interval', 'NOT_EVALUATED; no numerical assumption is made.', 'Supply the protection design and service basis.', 'Model output limitations'],
], 'Table C. Assumptions, missing data and required resolution.')}
<h3>10.2 Quality checks</h3>
<p>All worked-example values are inserted from public module calls and embedded with their calculation record. Displayed potential values use a consistent decimal precision; comparisons use unrounded module results. The rebuild test compares the complete generated file with the retained report and binds every displayed example number to a returned or derived value. A separate test compares all published explorer cells with public pair calls. The generator does not read the explorer data while it may be regenerated concurrently. Rendering is refused when matrix cells disagree with pair calls.</p>
<p>Source claims are limited to the cited records. Geometry is explanatory and dimensionless; no design drawing or exposure experiment is represented. Offline chart points support pointer and keyboard focus. Comment persistence requires the explicit save action; a browser without file-picker support downloads the sidecar for placement beside the report. Browser execution and print layout require separate review.</p>
<h3>10.3 Symbols and status legend</h3>
<p>V denotes volt. ΔV denotes the absolute difference of the material-group indices. I<sub>a</sub> and I<sub>b</sub> denote those indices; L denotes the environment allowable and B the marginal-band width. Anodic denotes the member assigned the larger index by the software. Cathodic denotes the other member. NOT_EVALUATED means that a physical conclusion or its evidence is absent.</p>
<p>The difference is within the software allowable for “OK”. The difference exceeds the software allowable within its marginal band for “MARGINAL”. The difference exceeds the software marginal band and protection is requested for “PROTECT”. These sentences describe software categories, not engineering acceptance.</p>
<details><summary>Engineering changes since the previous issue</summary><p>The current issue corrects presentation and reproducibility. Its engineering content is listed below.</p><ul id="changes-list"></ul></details>
<details><summary>Report comments</summary>
<p>Comments are retained in a separate file. Save the comments beside this HTML as <code>report.comments.json</code>; load that file when reopening the report. File-picker support depends on the browser. Without it, the download requires manual placement beside the report. No automatic save is performed.</p>
<form id="comment-form"><p><label for="comment-section">Section</label> <select id="comment-section" name="section">{options}</select></p>
<p><label for="comment-text">Comment</label><br><textarea id="comment-text" name="comment" rows="5" cols="40" required></textarea></p><button type="submit">Add comment</button></form>
<p><button id="save-comments" type="button">Save comments</button></p>
<p><label for="load-comments">Load comments</label> <input id="load-comments" type="file" accept=".json"></p>
<p id="comment-message" aria-live="polite">No comments are saved until Save comments is selected.</p><pre id="comments-preview">[]</pre></details>
</section></main></div>'''
    return (f'<!DOCTYPE html>\n<html lang="en" data-theme="light"><head><meta charset="utf-8">\n'
            '<meta name="viewport" content="width=device-width,initial-scale=1">\n'
            '<title>Corrosion module analysis</title>\n' + house_style() +
            '\n</head><body class="hs-doc doc">\n' + body +
            '\n<script id="calculation-data" type="application/json">' + json.dumps(data, sort_keys=True) + '</script>\n' +
            '<script id="change-data" type="application/json">' + json.dumps(CHANGES, sort_keys=True) + '</script>\n' +
            '<script>' + INTERACTION + '</script>\n</body></html>\n')


def build(output_dir: Path = DESTINATION) -> None:
    """Write the requested report and engineering change panel data."""
    output_dir.mkdir(parents=True, exist_ok=True)
    (output_dir / "report.html").write_text(render(), encoding="utf-8", newline="\n")
    (output_dir / "changes.json").write_text(json.dumps(CHANGES, indent=2) + "\n", encoding="utf-8", newline="\n")


def main() -> None:
    """Select the report output directory."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", type=Path, default=DESTINATION)
    args = parser.parse_args()
    build(args.output_dir)


if __name__ == "__main__":
    main()
