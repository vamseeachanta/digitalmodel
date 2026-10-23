# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Reproduce the standalone synthetic padeye analysis and change panel.

Run with the source package available:
python -m digitalmodel.lifting_lug.padeye_report --output docs/reports/padeye-lift-point
The calculation requires only the standard library. No solver is imported.
"""

from __future__ import annotations

import argparse
from dataclasses import replace
import hashlib
from html import escape
import json
from pathlib import Path

from .padeye_check import (
    Criterion,
    DirectWeld,
    LoadCase,
    PadeyePlate,
    PadeyeResult,
    ScreeningCriteria,
    Source,
    check_padeye,
)


# All numbers here are explicit synthetic inputs, not standard-derived limits.
# Their source is the report's input register (initial issue, 2026), section 10.1.
PLATE = PadeyePlate(20, 160, 60, 50, 100)
INPUT_SOURCE = Source("Synthetic analysis input register", "2026 initial issue", "10.1")
LOAD = LoadCase(100, 1.2, 1.5, INPUT_SOURCE)
WELD = DirectWeld(8, 300)
CRITERIA = ScreeningCriteria(
    Criterion(200, INPUT_SOURCE),
    Criterion(100, INPUT_SOURCE),
    Criterion(100, INPUT_SOURCE),
    Criterion(60, INPUT_SOURCE),
)
DATE = "2026-10-19"
CHANGES = [
    {
        "section": "basis",
        "change": "Initial synthetic basis with supplied comparison targets and unresolved class criteria.",
    },
    {
        "section": "geometry",
        "change": "Centred plate and balanced direct weld idealisation defined.",
    },
    {
        "section": "plate-results",
        "change": "Nominal bearing, net tension and shear demands calculated.",
    },
    {
        "section": "weld-results",
        "change": "Nominal weld comparison and effective throat requirement calculated.",
    },
]
SECTIONS = [
    ("summary", "Summary"),
    ("basis", "Basis"),
    ("geometry", "Geometry"),
    ("method", "Method"),
    ("plate-results", "Plate results"),
    ("weld-results", "Weld results"),
    ("way-forward", "Way forward"),
    ("validation", "Validation"),
    ("references", "References"),
    ("appendix", "Appendix"),
]


def sample_case() -> PadeyeResult:
    """Evaluate the explicit synthetic input register without class approval."""
    return check_padeye(PLATE, LOAD, CRITERIA, WELD)


def _table(headers: tuple[str, ...], rows: list[list[str]]) -> str:
    head = "".join(f"<th scope='col'>{escape(v)}</th>" for v in headers)
    body = "".join(
        "<tr>" + "".join(f"<td>{escape(v)}</td>" for v in row) + "</tr>" for row in rows
    )
    return f"<div class='scroll'><table><thead><tr>{head}</tr></thead><tbody>{body}</tbody></table></div>"


def _weld_chart() -> str:
    """Plot one computed quantity against explicit synthetic static tensions."""
    # Chart sample tensions are synthetic variants of the section 10.1 input.
    loads = (80, 100, 120)
    rows = [
        (
            p,
            check_padeye(PLATE, replace(LOAD, static_sling_load_kn=p), CRITERIA, WELD)
            .checks[-1]
            .demand_mpa,
        )
        for p in loads
    ]
    # Remaining constants are SVG display coordinates, not engineering values.
    dots = []
    for p, stress in rows:
        x, y = 90 + (p - 80) * 12, 300 - stress * 2.5
        title = (
            f"Static sling {p:.1f} kN; component: base weld; "
            f"demand {stress:.1f} MPa; limit 60.0 MPa (synthetic)"
        )
        dots.append(
            f"<circle tabindex='0' cx='{x:.1f}' cy='{y:.1f}' r='6' fill='#125c85'>"
            f"<title>{escape(title)}</title></circle>"
            f"<text x='{x:.1f}' y='325' text-anchor='middle'>{p:.1f}</text>"
        )
    return (
        "<svg viewBox='0 0 720 365' role='img' aria-label='Weld nominal resultant stress against static sling tension'>"
        "<title>Synthetic load sensitivity; hover over or focus on points for demand and target.</title>"
        "<path d='M65 35V300H650' fill='none' stroke='#364b59'/>"
        "<path d='M65 150H650' stroke='#aa4b20' stroke-dasharray='7 4'/>"
        "<text x='350' y='143'>Synthetic comparison target: 60.0 MPa</text>"
        "<text x='30' y='304'>0</text><text x='22' y='154'>60</text>"
        "<text x='15' y='54'>100</text>"
        "<polyline points='90,150 330,112.5 570,75' fill='none' stroke='#125c85'/>"
        + "".join(dots)
        + "<text x='265' y='352'>Static sling tension (kN)</text>"
        "<text x='12' y='22'>Nominal resultant stress (MPa)</text></svg>"
    )


SCHEMATIC = """<svg viewBox="0 0 880 470" role="img" aria-label="Centred plate, loaded hole, symmetric base weld and supporting plate">
<title>Single plate with straight loaded edge; centred axial pin load and balanced base attachment.</title>
<defs><marker id="arrow" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="6" markerHeight="6" orient="auto-start-reverse"><path d="M0 0L10 5L0 10Z" fill="#125c85"/></marker></defs>
<path d="M100 365V70H380V365" fill="#dce9ed" stroke="#125c85" stroke-width="3"/>
<circle cx="240" cy="225" r="53" fill="white" stroke="#125c85" stroke-width="3"/>
<circle cx="240" cy="225" r="44" fill="#bbc9d1" stroke="#364b59"/>
<path d="M240 225V25" stroke="#125c85" stroke-width="4" marker-end="url(#arrow)"/>
<text x="255" y="38">Axial sling load</text>
<path d="M100 300H380" stroke="#364b59" marker-start="url(#arrow)" marker-end="url(#arrow)"/>
<text x="135" y="323">Width: 160.0 mm</text>
<path d="M405 225V70M380 70H415M295 225H415" stroke="#364b59" fill="none"/>
<text x="415" y="145">Centre to edge:</text><text x="415" y="165">100.0 mm</text>
<path d="M187 225V70M293 225V70" stroke="#aa4b20" stroke-dasharray="6 4"/>
<text x="30" y="403">Front view: hole 60.0 mm; pin 50.0 mm</text>
<path d="M80 365H400V385H80Z" fill="#aebec7" stroke="#364b59"/>
<path d="M105 355H375M105 360H375" stroke="#aa4b20" stroke-width="3"/>
<text x="30" y="430">Balanced base weld; uniform direct throat demand</text>
<path d="M650 345V120H680V345" fill="#dce9ed" stroke="#125c85" stroke-width="3"/>
<path d="M580 365H750V385H580Z" fill="#aebec7" stroke="#364b59"/>
<path d="M620 365L650 335V365ZM680 335L710 365H680Z" fill="#d6a27c" stroke="#aa4b20"/>
<path d="M665 120V60" stroke="#125c85" stroke-width="4" marker-end="url(#arrow)"/>
<text x="570" y="100">Side view: plate 20.000 mm</text>
<text x="555" y="275">Effective throat: 8.000 mm</text>
<text x="535" y="410">Total effective length: 300.0 mm</text>
<text x="535" y="435">Supporting plate: strength not evaluated</text>
<text x="560" y="460">Schematic; dimensions govern, not scale</text>
</svg>"""


STYLE = """body{margin:0;background:#eef2f4;color:#233d4d;font:16px/1.6 system-ui,sans-serif}
main{max-width:1040px;margin:auto;background:white;padding:36px}h1,h2,h3{font-weight:500;line-height:1.3}
.title{font-size:32px;line-height:1.2}h2{margin-top:38px;border-bottom:1px solid #b8cbd4}
nav{display:flex;flex-wrap:wrap;gap:12px}a{color:#125c85}.notice{padding:14px;border-left:4px solid #aa4b20;background:#fff4e9}
table{width:100%;border-collapse:collapse;font-size:14px}th,td{text-align:left;padding:8px;border-bottom:1px solid #dce5e9}
th{font-weight:500}.scroll{overflow-x:auto}figure{margin:22px 0}svg{max-width:100%;height:auto}svg text{font:14px system-ui,sans-serif}
figcaption,.caption{font-size:14px;color:#526774}textarea{width:95%;min-height:80px}button,select,input{font:inherit;margin:6px 6px 6px 0}
button{padding:6px 10px}pre{white-space:pre-wrap;overflow-wrap:anywhere;font-size:13px}.comment{border-left:3px solid #b8cbd4;padding:8px}
@media(max-width:650px){main{padding:18px}.title{font-size:26px}table{font-size:12px}}"""


REVIEW_SCRIPT = r"""(() => {
  'use strict';
  const config = JSON.parse(document.getElementById('review-config').textContent);
  const el = id => document.getElementById(id);
  const ids = new Set(config.sections.map(s => s[0]));
  const state = {report: 'Padeye and lift point analysis', revision: 0,
                 content_digest: config.content_digest, comments: []};
  const status = text => { el('comment-status').textContent = text; };
  for (const section of config.sections) {
    const option = document.createElement('option');
    option.value = section[0]; option.textContent = section[1];
    el('comment-section').appendChild(option);
  }
  const show = () => {
    el('comment-list').textContent = '';
    for (const comment of state.comments) {
      const row = document.createElement('p'); row.className = 'comment';
      row.textContent = `${comment.section}: ${comment.text}`;
      el('comment-list').appendChild(row);
    }
  };
  el('add-comment').onclick = () => {
    const text = el('comment-text').value.trim();
    if (!text) return status('Enter a comment.');
    state.comments.push({section: el('comment-section').value, text,
                         recorded_at: new Date().toISOString()});
    el('comment-text').value = ''; show();
    status('Comment added in memory. Save comments before closing.');
  };
  el('load-comments').onchange = async () => {
    try {
      const file = el('load-comments').files[0]; if (!file) return;
      const incoming = JSON.parse(await file.text());
      if (incoming.content_digest !== state.content_digest || incoming.revision !== state.revision ||
          incoming.report !== state.report || !Array.isArray(incoming.comments) ||
          incoming.comments.some(c => !c || !ids.has(c.section) || typeof c.text !== 'string' ||
                                     typeof c.recorded_at !== 'string'))
        throw new Error('Comments do not match this report or contain invalid fields.');
      const existing = new Set(state.comments.map(c => JSON.stringify(c)));
      for (const c of incoming.comments) if (!existing.has(JSON.stringify(c))) {
        state.comments.push(c); existing.add(JSON.stringify(c));
      }
      show(); status('Matching comments loaded. Save to retain the combined copy.');
    } catch (error) { status(error.message); }
    el('load-comments').value = '';
  };
  el('save-comments').onclick = async () => {
    try {
      const data = JSON.stringify(state, null, 2) + '\n';
      if (typeof window.showSaveFilePicker === 'function') {
        const handle = await window.showSaveFilePicker({suggestedName: 'report.comments.json',
          types: [{description: 'Report comments', accept: {'application/json': ['.json']}}]});
        const writable = await handle.createWritable();
        await writable.write(data); await writable.close();
        if (await (await handle.getFile()).text() !== data) throw new Error('Saved comments differ on read-back.');
        return status('Comments saved and read back. Keep the JSON beside report.html.');
      }
      const url = URL.createObjectURL(new Blob([data], {type: 'application/json'}));
      const link = document.createElement('a'); link.href = url; link.download = 'report.comments.json';
      link.click(); setTimeout(() => URL.revokeObjectURL(url), 1000);
      status('Download requested. Place report.comments.json beside report.html; reload it to verify.');
    } catch (error) { status(error.message); }
  };
})();"""


def render_report() -> str:
    """Render one report from this calculation's synthetic inputs and results."""
    result = sample_case()
    weld = result.checks[-1]
    nav = "".join(f"<a href='#{key}'>{label}</a>" for key, label in SECTIONS)
    results = _table(
        (
            "Component / mode",
            "Area (mm²)",
            "Demand (MPa)",
            "Synthetic target (MPa)",
            "Comparison",
        ),
        [
            [
                c.mode.replace("_", " "),
                f"{c.area_mm2:.1f}",
                f"{c.demand_mpa:.1f}",
                f"{c.criterion.allowable_mpa:.1f}",
                c.status.value,
            ]
            for c in result.checks
        ],
    )
    # Independent hand values, derived in test_padeye_check's docstring:
    # F=180000 N; A=1000,2000,2800,2400 mm2 (OpenStax 2016, 12.3).
    hand = (180, 90, 450 / 7, 75)
    validation = _table(
        (
            "Mode",
            "Hand calculation (MPa)",
            "Calculated (MPa)",
            "Difference from hand (%)",
        ),
        [
            [
                c.mode.replace("_", " "),
                f"{expected:.1f}",
                f"{c.demand_mpa:.1f}",
                f"{100 * (c.demand_mpa - expected) / expected:.1f}",
            ]
            for c, expected in zip(result.checks, hand)
        ],
    )
    inputs = _table(
        ("Quantity", "Value", "Basis"),
        [
            ["Plate thickness; width", "20.000 mm; 160.0 mm", "Synthetic geometry"],
            ["Hole; pin diameter", "60.0 mm; 50.0 mm", "Synthetic geometry"],
            ["Hole centre to loaded edge", "100.0 mm", "Straight edge; centred hole"],
            ["Static tension in the selected leg", "100.0 kN", "Synthetic load"],
            [
                "Dynamic amplification; skew factor",
                "1.2; 1.5",
                "Explicit synthetic factors; no rule minima inferred",
            ],
            [
                "Effective weld throat; total effective length",
                "8.000 mm; 300.0 mm",
                "Balanced direct load; uniform resultant stress",
            ],
            [
                "Bearing; net tension; tear-out targets",
                "200.0; 100.0; 100.0 MPa",
                "Synthetic comparison targets, not design allowables",
            ],
            [
                "Weld resultant target",
                "60.0 MPa",
                "Synthetic comparison target, not a design allowable",
            ],
            [
                "ABS edition, clauses, material and inspection basis",
                "Not supplied",
                "Acceptance NOT_EVALUATED",
            ],
        ],
    )
    exclusions = "".join(f"<li>{escape(v)}.</li>" for v in result.exclusions)
    changes = "".join(f"<li>{escape(c['change'])}</li>" for c in CHANGES)
    page = f"""<!doctype html>
<html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">
<title>Padeye and lift point analysis</title><style>{STYLE}</style></head>
<body><main><header><p class="title">Padeye and lift point analysis</p>
<p>Draft for client review</p><p>Synthetic example · Revision 0 · {DATE}</p></header>
<nav aria-label="Report contents">{nav}</nav>
<section id="summary"><h2>1. Summary</h2>
<p>The lift point is a single plate with a centred pin hole and a balanced welded base.
Analysis is performed to determine nominal plate stresses and direct weld throat demand;
key results are summarized below.</p>
<ul><li>The supplied synthetic plate targets are met at the illustrative load.
<ul><li>The calculated bearing, net tension and tear-out demands are 180.0, 90.0 and 64.3 MPa.</li></ul></li>
<li>The illustrative weld target is exceeded.
<ul><li>The calculated demand is {weld.demand_mpa:.1f} MPa against a synthetic 60.0 MPa target.
An effective throat of {weld.required_throat_mm:.3f} mm is required for that target at the same load and length.</li></ul></li>
<li>A lifting capacity remains unassigned.
<ul><li>American Bureau of Shipping (ABS) acceptance is NOT_EVALUATED because applicable rule criteria and the complete connection basis are absent.</li></ul></li></ul>
<p>Revision history</p>
<table><thead><tr><th>Revision</th><th>Date</th><th>Change and cause</th></tr></thead><tbody>
<tr><td>0</td><td>{DATE}</td><td>Initial synthetic example to demonstrate nominal stress calculations and missing-criterion handling.</td></tr></tbody></table>
</section>
<section id="basis"><h2>2. Analysis basis</h2>
<p>A centred axial load is applied in the plane of a uniform plate. Direct force is transferred
through a balanced base weld; connection moments are excluded by this idealisation.</p>
<p class="notice">ASSUMED - to be confirmed: all geometry, loads, factors and comparison targets
are synthetic inputs. The targets demonstrate comparisons and do not represent ABS design allowables.</p>
<p>Force divided by area is used for nominal stress. International Maritime Organization (IMO)
lifting guidance requires a design basis acceptable to the relevant Administration;
nominal stress calculations alone do not establish that basis. ABS Publication 152 is identified
as a candidate rule source, but its applicable edition and design clauses remain unresolved.</p>
<p>Each supplied target is used only with its matching stress definition. A result at or below
its supplied target is marked PASS for that comparison; a missing criterion is marked NOT_EVALUATED.
No material strength, code coefficient or acceptance limit is inferred.</p></section>
<section id="geometry"><h2>3. Physical system and idealisation</h2>
<p>A rectangular plate head with a straight loaded edge is represented. Plate width is measured
across the load, and edge distance is measured along the load from the hole centre.</p>
<figure>{SCHEMATIC}<figcaption>Figure 1. Physical load path and idealised attachment.
The pin and hole are concentric for dimension display; clearance contact is not modelled.
The support reaction is equal and opposite to the applied axial force. No mesh is used.</figcaption></figure>
</section>
<section id="method"><h2>4. Calculation method</h2>
<p>Nominal areas are formed from the entered effective dimensions. The following symbols
are defined for the independent force and area calculation.</p>
<p>P is the factored sling force; S is the static tension in the selected sling leg;
D is the dynamic amplification input; K is the skew input. The force is P = S × D × K.
Here P = 100.0 × 1.2 × 1.5 = {result.design_load_kn:.1f} kN.</p>
<p>For thickness t, plate width w, pin diameter d, hole diameter h and centre-to-edge distance e,
the projected bearing area is d × t, the net tensile area is (w − h) × t,
and the nominal two-plane shear area is 2 × (e − h/2) × t.
The shear expression is a geometric screening idealisation; empirical lug interaction is excluded.</p>
<p>The nominal stress is P divided by the corresponding area, with force in newtons and area in square millimetres
giving megapascals (MPa). For effective throat a and total effective weld length L, the weld area is a × L.
The weld quantity is a nominal resultant stress on the throat, rather than resolved shear stress.</p>
<p>For a supplied weld resultant target q, the required effective throat is a = P/(L × q).
Its conversion to a weld leg, minimum weld size, effective length and directional strength remain outside this calculation.</p>
</section>
<section id="plate-results"><h2>5. Plate results</h2>
<p>The three plate demands are calculated at a factored sling load of {result.design_load_kn:.1f} kN.
They are below the supplied synthetic targets, while class acceptance remains NOT_EVALUATED.</p>
<p>The projected bearing area is 1,000.0 mm², the net tensile area is 2,000.0 mm²,
and the nominal shear area is 2,800.0 mm². These areas establish average stresses only;
local contact, hoop stress and combined failure mechanisms remain unresolved.</p></section>
<section id="weld-results"><h2>6. Weld results</h2>
<p>A nominal resultant stress of {weld.demand_mpa:.1f} MPa is calculated for the entered throat and effective length.
The synthetic 60.0 MPa comparison target is exceeded.</p>
<figure>{_weld_chart()}<figcaption>Figure 2. Direct weld stress against static sling tension;
dynamic amplification and skew remain fixed. The horizontal target is synthetic and is not an ABS criterion.</figcaption></figure>
<p>At the illustrated load, an effective throat of {weld.required_throat_mm:.3f} mm is required
to meet the synthetic target. The nominal resultant comparison does not evaluate weld directional interaction.</p></section>
<section id="way-forward"><h2>7. Way forward</h2>
<p>The illustrative weld shortfall is addressed by a calculated effective throat requirement.
The calculation is conditional on the same direct load idealisation and synthetic target.</p>
<p>Increase the effective throat to at least {weld.required_throat_mm:.3f} mm for the illustrative comparison,
or limit the factored force to 144.0 kN for the entered throat and synthetic target.
These alternatives follow from force divided by throat area; neither assigns a working load.</p>
<p>Before assigning a lifting capacity, establish the applicable class edition and clauses,
material certificates, connection geometry, directional weld criteria, pin and support checks,
and the required examination and load-test basis. The cited lifting guidance supplies the rationale
for obtaining an applicable design and examination basis.</p></section>
<section id="validation"><h2>8. Validation</h2>
<p>The calculated stresses are compared with independent hand calculations using the entered dimensions.
Arithmetic agreement is established for this example; physical qualification is not established.</p>
<p>The independent areas are 50 × 20 = 1,000 mm², (160 − 60) × 20 = 2,000 mm²,
2 × (100 − 30) × 20 = 2,800 mm² and 8 × 300 = 2,400 mm².
With 180,000 N, the independent demands are 180.0, 90.0, 64.3 and 75.0 MPa.
The independent throat calculation is 180,000/(300 × 60) = 10.000 mm.</p>
{validation}<p class="caption">Table 1. Arithmetic comparison, with differences calculated before rounding.</p>
<p>Automated checks cover absent criteria, retained failures, equality at a supplied target,
invalid geometry, nonfinite inputs, zero force and overflow. Reproduction is checked against
the issued page and its change panel.</p></section>
<section id="references"><h2>9. References</h2>
<p>Mechanics references and lifting guidance are distinguished from the unresolved class criteria.
No empirical lug strength factor is taken from the references.</p>
<h3>9.1 Source coverage</h3><p>The public mechanics text and lifting guideline were read.
The ABS rule endpoint could not be resolved; no ABS design equation or allowable is claimed.</p>
<h3>9.2 Source locators</h3><ul>
<li><a href="https://openstax.org/books/university-physics-volume-1/pages/12-3-stress-strain-and-elastic-modulus">OpenStax, University Physics Volume 1 (2016), section 12.3</a>:
nominal normal and shear stress as force divided by area.</li>
<li><a href="https://engineeringlibrary.org/reference/lug-analysis-air-force-stress-manual">Stress Analysis Manual (October 1986), section 9.2, equations 9-3a and 9-6a</a>:
projected bearing and net-section geometry; the empirical strength factors are excluded.</li>
<li><a href="https://ww2.eagle.org/content/dam/eagle/Images/rules-and-resources/regulatory-updates/solas-requirements/guidelines-hyperlinks/msc1-circ1663-guidelines-for-lifting-appliances%20(4).pdf">IMO MSC.1/Circ.1663 (2023), annex 3.1 and 3.2</a>:
design, construction, installation, testing and examination basis.</li>
<li><a href="https://ww2.eagle.org/en/rules-and-resources/regulatory-updates/SOLAS-requirements-on-board-ships.html">ABS public lifting guidance</a> identifies Publication 152,
Guide for Certification of Lifting Appliances. The linked rule content was unavailable on {DATE}; edition and clauses are unconfirmed.</li>
<li>Reproduction: <code>python -m digitalmodel.lifting_lug.padeye_report --output docs/reports/padeye-lift-point</code>,
with the source package available. Independent checks: <code>tests/workflows/test_padeye_check.py</code>
and <code>tests/workflows/test_padeye_report.py</code>. Change panel: <code>changes.json</code>.</li>
</ul></section>
<section id="appendix"><h2>10. Appendix</h2>
<p>The synthetic inputs and mode-level comparisons are retained below.
Missing design data are separated from true numerical zeros.</p>
<h3>10.1 Data register and comparisons</h3>
<p>ASSUMED - to be confirmed: each numerical entry is an explicit synthetic input for this example.
No measured or previous-design data are used.</p>{inputs}
<p class="caption">Table 2. Synthetic input register; the stress targets are illustrative supplied inputs.</p>
{results}<p class="caption">Table 3. Nominal comparisons against the synthetic targets only.</p>
<p>The following exclusions remain NOT_EVALUATED:</p><ul>{exclusions}</ul>
<h3>10.2 Pre-flight evidence</h3>
<p>Units, symbols, input provenance, area derivations and reference coverage are stated.
The page is self-contained; both figures are embedded and the chart target is labelled synthetic.
The arithmetic comparator is independent of the calculation implementation.</p>
<p>The comment controls are checked without a browser. Interactive rendering and file-picker
behaviour require a browser check before external issue. Class acceptance, as-built dimensions,
material evidence and examination records remain absent.</p>
<h3>10.3 Comments and changes</h3>
<p>Comments are entered by report section and retained in a separate file. Choose the folder containing
this report when saving. If a file picker is unavailable, a download is requested; place the downloaded
comments beside the report and reload them to confirm the saved copy. Unsaved comments are lost on closing.</p>
<label>Section <select id="comment-section"></select></label>
<label for="comment-text">Comment</label><textarea id="comment-text"></textarea>
<button id="add-comment" type="button">Add comment</button>
<button id="save-comments" type="button">Save comments beside report</button>
<label>Load comments <input id="load-comments" type="file" accept="application/json,.json"></label>
<p id="comment-status" role="status">No comments saved.</p><div id="comment-list"></div>
<details><summary>Change panel</summary><ul>{changes}</ul></details>
<noscript>Comment controls require JavaScript. The analysis remains readable.</noscript>
</section></main>__REVIEW__</body></html>
"""
    config = {
        "sections": SECTIONS,
        "content_digest": hashlib.sha256(page.encode()).hexdigest(),
    }
    review = (
        '<script id="review-config" type="application/json">'
        + json.dumps(config, sort_keys=True)
        + "</script><script>"
        + REVIEW_SCRIPT
        + "</script>"
    )
    return page.replace("__REVIEW__", review)


def write_report(directory: Path) -> None:
    """Write the standalone HTML and a digest-bound, deterministic change panel."""
    directory.mkdir(parents=True, exist_ok=True)
    page = render_report()
    (directory / "report.html").write_text(page, encoding="utf-8")
    changes = {
        "report": "Padeye and lift point analysis",
        "revision": 0,
        "date": DATE,
        "report_digest": hashlib.sha256(page.encode()).hexdigest(),
        "changes": CHANGES,
    }
    (directory / "changes.json").write_text(
        json.dumps(changes, indent=2) + "\n", encoding="utf-8"
    )


def main() -> None:
    """Generate the synthetic example at the requested destination."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, required=True)
    write_report(parser.parse_args().output)


if __name__ == "__main__":
    main()
