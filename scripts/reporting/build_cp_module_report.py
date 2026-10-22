# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Build the standalone cathodic protection module analysis from live results.

Execute with the repository source on the Python import path. The default
output is the report directory; --output permits a scratch rebuild. No network,
external citation store or solver is accessed by this builder.
"""

from __future__ import annotations

import argparse
from copy import deepcopy
import hashlib
from html import escape
import json
from pathlib import Path
import re
from typing import Any

import yaml

from digitalmodel.cathodic_protection import _kernels as kernel
from digitalmodel.cathodic_protection.engine_adapter import run_cathodic_protection

ROOT = Path(__file__).resolve().parents[2]
EXAMPLE = ROOT / "tests/fixtures/cathodic_protection/workflow_inputs/published_jacket_84m.yml"
GEOMETRY = ROOT / "tests/fixtures/cathodic_protection/workflow_inputs/reference_jacket_60m_members.yml"
STYLE = ROOT / "src/digitalmodel/reporting/assets/calc_report_format.html"
OUTPUT = ROOT / "docs/reports/modules/cp"
PAPER = "https://www.hillpublisher.com/UpFile/202308/20230803150221.pdf"
GEOMETRY_PAPER = "https://doi.org/10.5194/wes-8-807-2023"
DATE = "2026-10-22"
ZONES = {
    "coated-seawater": "Coated seawater steel",
    "upper-bare-seawater": "Upper bare seawater steel",
    "lower-bare-seawater": "Lower bare seawater steel",
    "buried-steel": "Buried steel",
}
SECTIONS = (
    ("summary", "1 Executive summary"),
    ("scope", "2 Scope and capability"),
    ("basis", "3 Design basis and acceptance criteria"),
    ("theory", "4 Theory and calculation sequence"),
    ("worked-example", "5 Worked public example"),
    ("results", "6 Detailed results and validation evidence"),
    ("limits", "7 Limits of use"),
    ("recommendations", "8 Recommendations"),
    ("appendix", "9 Evidence appendix"),
    ("glossary", "10 Symbols and units"),
)
CHANGES = {
    "document": "cathodic-protection-module-analysis",
    "revision": 0,
    "date": DATE,
    "changes": [
        {"section": "basis", "description": "Public phase-density inputs are distinguished from assumed life and alloy properties."},
        {"section": "results", "description": "Aggregate current and mass requirements are evaluated separately from spatial protection."},
        {"section": "recommendations", "description": "The aggregate anode increase is recalculated; field retrofit sizing remains unevaluated."},
    ],
}


def calculate() -> dict[str, Any]:
    """Call the production entry point for the published and resized cases."""
    cfg = yaml.safe_load(EXAMPLE.read_text(encoding="utf-8"))
    original = deepcopy(cfg["inputs"])
    results = run_cathodic_protection(deepcopy(cfg))["results"]
    rec = results["current_output_verification"]["recommended_anode_count"]
    cfg["inputs"]["anode"]["count"] = rec
    rechecked = run_cathodic_protection(cfg)["results"]
    if rechecked["status"]["result"] != "PASS":
        raise ValueError("The resized aggregate arrangement does not pass")
    checks = results["current_output_verification"]
    if (results["status"]["result"] != "FAIL"
            or checks["governing_case"] != "initial"
            or checks["initial_meets_demand"]
            or not checks["mass_meets_requirement"]
            or not checks["final_meets_demand"]):
        raise ValueError("The documented initial-current shortfall has changed; revise the analysis")
    # Public reporting may use only the assumptions already declared in this
    # fixture. Do not silently substitute a changed material table/default.
    assumptions = original["structure"]["verification_evidence"]["assumptions"]
    patterns = {
        "capacity": r"Capacity (\d+(?:\.\d+)?) Ah/kg",
        "utilisation": r"utilisation (\d+(?:\.\d+)?)",
        "density": r"(\d+(?:\.\d+)?) kg/m³",
    }
    assumed = {}
    for key, pattern in patterns.items():
        match = re.search(pattern, assumptions)
        if match is None:
            raise ValueError(f"The declared {key} assumption is missing")
        assumed[key] = float(match[1])
    req = results["anode_requirements"]
    if (assumed["capacity"] != req["electrochemical_capacity_Ah_kg"]
            or assumed["utilisation"] != req["utilization_factor"]
            or assumed["density"] != kernel.ANODE_DENSITY_ALZNI):
        raise ValueError("The module material defaults differ from the declared assumptions")
    return {"inputs": original, "results": results, "rechecked": rechecked,
            "assumed": assumed}


def quantities(data: dict[str, Any]) -> dict[str, int | float]:
    """Flatten numerical evidence without publishing implementation strings."""
    out: dict[str, int | float] = {}

    def walk(node: Any, key: str) -> None:
        if isinstance(node, dict):
            for name, value in node.items():
                walk(value, f"{key}.{name}" if key else name)
        elif isinstance(node, list):
            for index, value in enumerate(node):
                walk(value, f"{key}.{index}")
        elif isinstance(node, (int, float)) and not isinstance(node, bool):
            out[key] = node

    walk(data, "")
    for zone in ZONES:
        for phase in ("initial", "mean", "final"):
            key = f"results.current_demand_A.{zone}.i_{phase}_A_m2"
            # Metric-prefix conversion, also defined in anode_sizing._MA_PER_A.
            out[f"density_mA_m2.{zone}.{phase}"] = out[key] * 1000
    model = yaml.safe_load(GEOMETRY.read_text(encoding="utf-8"))["structural_model"]
    out["geometry.water_depth_m"] = model["water_depth_m"]
    out["geometry.member_count"] = len(model["members"])
    out["geometry.joint_count"] = len(model["joints"])
    out["chart.origin"] = 0
    ver = data["results"]["current_output_verification"]
    out["derived.additional_anodes"] = ver["recommended_anode_count"] - ver["anode_count"]
    out["derived.provided_mass_kg"] = ver["anode_count"] * data["results"]["anode_requirements"]["individual_mass_kg"]
    # Targets are source figures, not calculation oracles. Keep their units
    # and source precision separate from the live module's return values.
    targets = data["inputs"]["structure"]["verification_evidence"]["targets"]
    mapping = {
        "initial_demand": data["results"]["current_demand_A"]["total_initial_A"],
        "mean_demand": data["results"]["current_demand_A"]["total_mean_A"],
        "final_demand": data["results"]["current_demand_A"]["total_final_A"],
        "initial_output": ver["anode_current_output_initial_A"],
        "final_output": ver["anode_current_output_final_A"],
        "count": ver["recommended_anode_count"],
    }
    for row in targets:
        name = row["quantity"]
        out[f"comparison.{name}.reference"] = row["printed"]
        out[f"comparison.{name}.calculated"] = mapping[name]
        out[f"comparison.{name}.difference_percent"] = abs(100 * (mapping[name] / row["printed"] - 1))
    return out


def format_number(value: int | float, places: int = 1) -> str:
    """Use engineering precision and preserve small nonzero quantities."""
    if value and round(abs(value), places) == 0:
        bound = 10 ** -places
        return ("−" if value < 0 else "") + f"<{bound:.{places}f}"
    return f"{value:,.{places}f}".replace("-", "−")


def number(values: dict[str, int | float], key: str, places: int = 1,
           tag: str = "span", attrs: str = "") -> str:
    """Bind each visible quantity to the live evidence record."""
    value = values[key]
    return (f'<{tag} data-key="{key}" data-value="{value}" data-places="{places}"{attrs}>'
            f'{escape(format_number(value, places))}</{tag}>')


def table(headers: list[str], rows: list[list[str]]) -> str:
    """Render actual table columns using only prepared safe cell content."""
    return ('<div class="tbl"><table><thead><tr>'
            + ''.join(f'<th scope="col">{h}</th>' for h in headers)
            + '</tr></thead><tbody>'
            + ''.join('<tr>' + ''.join(f'<td>{c}</td>' for c in row) + '</tr>' for row in rows)
            + '</tbody></table></div>')


def geometry_figure(values: dict[str, int | float]) -> str:
    """Draw the supplied public member coordinates, keeping structures distinct."""
    model = yaml.safe_load(GEOMETRY.read_text(encoding="utf-8"))["structural_model"]
    joints = {j["id"]: j["coordinates_m"] for j in model["joints"]}
    props = {p["id"]: p for p in model["property_sets"]}
    depth = model["water_depth_m"]
    top = max(j[2] for j in joints.values())
    scale = 5.5

    def xy(point: list[float]) -> tuple[float, float]:
        return 220 + point[1] * scale, 45 + (top - point[2]) * scale

    water_y = xy([0, 0, 0])[1]
    bottom_y = xy([0, 0, -depth])[1]
    lines = []
    for member in model["members"]:
        a, b = (xy(joints[i]) for i in member["joint_ids"])
        diameter = props[member["property_set_id"]]["diameter_m"]
        lines.append(f'<line x1="{a[0]:.2f}" y1="{a[1]:.2f}" x2="{b[0]:.2f}" y2="{b[1]:.2f}" stroke-width="{diameter * scale:.2f}"/>')
    return f'''<figure class="card"><svg viewBox="0 0 700 510" role="img" aria-labelledby="geometry-title geometry-desc">
<title id="geometry-title">Public jacket member elevation</title>
<desc id="geometry-desc">Every member from the supplied member schedule is projected into the vertical plane. Tubular diameter controls line width. This is a separate structure from the worked cathodic protection example.</desc>
<rect x="80" y="{water_y:.2f}" width="550" height="{bottom_y-water_y:.2f}" fill="var(--ro-soft)"/>
<g stroke="var(--ink-2)" opacity="0.8">{''.join(lines)}</g>
<g stroke="var(--cfd)" stroke-dasharray="7 4"><line x1="60" x2="650" y1="{water_y:.2f}" y2="{water_y:.2f}"/><line x1="60" x2="650" y1="{bottom_y:.2f}" y2="{bottom_y:.2f}"/></g>
<g fill="var(--ink)" font-size="13"><text x="400" y="{water_y-10:.2f}">Still-water level</text>
<text x="400" y="{water_y+30:.2f}">Seawater exposure</text><text x="400" y="{bottom_y-10:.2f}">Mudline; axes terminate here</text>
<text x="400" y="55">Above-water steel</text><text x="400" y="76">Splash boundary unresolved</text>
<text x="80" y="490">Conductors and installed pile geometry are not supplied.</text></g></svg>
<figcaption>Figure A. A coordinate-derived jacket elevation is shown at one scale, including the projected legs and braces.
Water depth is {number(values, 'geometry.water_depth_m')} m; {number(values, 'geometry.member_count', 0)} members connect
{number(values, 'geometry.joint_count', 0)} joints. Different structures are represented by this geometry illustration and the worked example.
No areas or anode placements are transferred between them. Geometry attribution and source limitations are recorded in <a href="#sources">the source register</a>.</figcaption></figure>'''


def count_chart(values: dict[str, int | float]) -> str:
    """Plot required count against the supplied count with keyboard tooltips."""
    prefix = "results.current_output_verification."
    names = [("Mass", "count_by_mass"), ("Initial current", "count_by_initial_current"),
             ("Final current", "count_by_final_current")]
    limit = values[prefix + "anode_count"]
    maximum = max(values[prefix + k] for _, k in names) * 1.16
    y = lambda v: 330 - 260 * v / maximum
    items = []
    for x, (label, key) in zip((160, 360, 560), names):
        value = values[prefix + key]
        tooltip = (f"Case: published jacket; Component: {label}; Demand: {value} anodes; "
                   f"Limit: {limit} supplied anodes; Units: anodes")
        items.append(f'''<g tabindex="0" data-tooltip="{tooltip}" aria-label="{tooltip}"><title>{tooltip}</title>
<rect x="{x-48}" y="{y(value):.2f}" width="96" height="{330-y(value):.2f}" fill="var(--ro)"/>
{number(values, prefix+key, 0, 'text', f' x="{x}" y="{y(value)-12:.2f}" text-anchor="middle" fill="var(--ink)"')}
<text x="{x}" y="355" text-anchor="middle" fill="var(--ink)">{label}</text></g>''')
    return f'''<figure class="card"><svg id="count-chart" viewBox="0 0 740 410" role="img" aria-labelledby="count-title count-desc">
<title id="count-title">Anode requirement by governing criterion</title>
<desc id="count-desc">Bars show required anode counts. The dashed supplied-count limit is exceeded by initial current demand. The recommended count is rechecked in the results.</desc>
<line x1="70" x2="690" y1="330" y2="330" stroke="var(--ink)"/>
<line class="limit" x1="70" x2="690" y1="{y(limit):.2f}" y2="{y(limit):.2f}" stroke="var(--proj)" stroke-width="3" stroke-dasharray="8 5"/>
<text x="70" y="35" fill="var(--ink)">Required count (anodes)</text>
{number(values, 'chart.origin', 0, 'text', ' x="40" y="335" fill="var(--ink)"')}
{''.join(items)}
<text x="80" y="390" fill="var(--ink)">Dashed line: supplied count limit</text>
{number(values, prefix+'anode_count', 0, 'text', ' x="530" y="390" fill="var(--ink)"')}
</svg><p id="chart-readout" aria-live="polite">Hover over a bar or focus it with the keyboard to read demand and limit.</p>
<figcaption>Figure B. Count is evaluated by mass and fresh and depleted current output. Each bar is compared with the same supplied count.
The initial-current shortfall is resolved in the aggregate model by the increase in the recommendations.</figcaption></figure>'''


UI_SCRIPT = r'''
(() => {
  "use strict";
  const el = id => document.getElementById(id);
  const changes = JSON.parse(el("changes-data").textContent);
  for (const item of changes.changes) {
    const row = document.createElement("li"); row.textContent = item.description;
    el("change-list").append(row);
  }
  document.querySelectorAll("[data-tooltip]").forEach(point => {
    for (const name of ["mouseenter", "focus"]) point.addEventListener(name, () => {
      el("chart-readout").textContent = point.dataset.tooltip;
    });
  });
  const fingerprint = el("document-key").content;
  const storageKey = "cp-analysis-comments-" + fingerprint;
  let comments = [], quote = "";
  const status = message => { el("comment-status").textContent = message; };
  const valid = data => data && data.document === fingerprint && Array.isArray(data.comments)
    && data.comments.every(c => c && typeof c.section === "string" && typeof c.text === "string"
      && typeof c.quote === "string" && Array.from(el("comment-section").options).some(o => o.value === c.section));
  function render() {
    el("comment-list").replaceChildren();
    comments.forEach(c => {
      const item = document.createElement("li");
      item.textContent = c.section + (c.quote ? " — “" + c.quote + "”" : "") + ": " + c.text;
      el("comment-list").append(item);
    });
  }
  function persist() {
    try { localStorage.setItem(storageKey, JSON.stringify({document:fingerprint, comments})); return true; }
    catch (_) { status("Browser storage is unavailable. Save comments before closing."); return false; }
  }
  try {
    const saved = JSON.parse(localStorage.getItem(storageKey));
    if (valid(saved)) comments = saved.comments;
  } catch (_) { status("Browser storage is unavailable. Save comments before closing."); }
  render();
  document.addEventListener("selectionchange", () => {
    const selection = window.getSelection();
    if (selection && selection.rangeCount && selection.toString().trim()) {
      const node = selection.anchorNode.parentElement.closest("main section");
      if (node) { quote = selection.toString().trim(); el("comment-section").value = node.id; }
    }
  });
  el("add-comment").addEventListener("click", () => {
    const text = el("comment-text").value.trim();
    if (!text) { status("Enter a comment before adding it."); return; }
    comments.push({section:el("comment-section").value, quote, text});
    el("comment-text").value = ""; quote = ""; const stored = persist(); render();
    if (stored) status("Comment retained in this browser. Save the comment file beside the report.");
  });
  el("save-comments").addEventListener("click", async () => {
    const blob = new Blob([JSON.stringify({document:fingerprint, revision:changes.revision, comments}, null, 2) + "\n"], {type:"application/json"});
    if (window.showSaveFilePicker) {
      try {
        const handle = await window.showSaveFilePicker({suggestedName:"comments.json", types:[{description:"Comments", accept:{"application/json":[".json"]}}]});
        const writable = await handle.createWritable(); await writable.write(blob); await writable.close();
        status("Comments saved. Retain the comment file beside this report."); return;
      } catch (error) { if (error.name === "AbortError") return; }
    }
    const url = URL.createObjectURL(blob), link = document.createElement("a");
    link.href = url; link.download = "comments.json"; link.click();
    setTimeout(() => URL.revokeObjectURL(url), 1000);
    status("Comment file downloaded. Save it beside this report.");
  });
  el("load-comments").addEventListener("change", async event => {
    try {
      const file = event.target.files[0]; if (!file) return;
      const loaded = JSON.parse(await file.text());
      if (!valid(loaded)) throw new Error("Comment file does not match this report or has invalid content.");
      const seen = new Set(comments.map(c => JSON.stringify(c)));
      for (const c of loaded.comments) if (!seen.has(JSON.stringify(c))) {
        comments.push({section:c.section, quote:c.quote, text:c.text}); seen.add(JSON.stringify(c));
      }
      const stored = persist(); render();
      if (stored) status("Matching comments loaded; current comments retained.");
    } catch (_) { status("Comment file does not match this report or has invalid content."); }
  });
})();
'''


def render(data: dict[str, Any]) -> str:
    """Write the complete engineering narrative around live calculation values."""
    values = quantities(data)
    n = lambda key, places=1: number(values, key, places)
    v = "results.current_output_verification."
    r = "results.anode_requirements."
    d = "results.current_demand_A."
    src = '<a href="#sources">Source register</a>'
    zone_rows, demand_rows = [], []
    for index, (zone, label) in enumerate(ZONES.items()):
        p = f"inputs.structure.zones.{index}."
        zone_rows.append([label, n(p+'area_m2'),
                          *[n('density_mA_m2.'+zone+'.'+phase) for phase in ('initial', 'mean', 'final')],
                          'Public input basis; ' + src])
        demand_rows.append([label, *[n(d+zone+'.I_'+phase+'_A') for phase in ('initial', 'mean', 'final')],
                            'Table A; ' + src])
    basis_rows = [
        ["Design life", n('inputs.design_data.design_life') + " years", "ASSUMED - to be confirmed", "Existing example basis; public paper does not give life."],
        ["Seawater temperature", n('inputs.environment.seawater_temperature_C') + " °C", "ASSUMED - to be confirmed", "Existing example basis; published effective densities override the climate lookup."],
        ["Alloy density", n('assumed.density', 0) + " kg/m³", "ASSUMED - to be confirmed", "Existing example basis; used for the equivalent cylinder."],
        ["Electrochemical capacity", n('assumed.capacity', 0) + " Ah/kg", "ASSUMED - to be confirmed", "Existing example basis; equality with the active module value is checked."],
        ["Utilisation fraction", n('assumed.utilisation', 2), "ASSUMED - to be confirmed", "Existing example basis; equality with the active module value is checked."],
        ["Net alloy interpretation", n('inputs.anode.individual_anode_mass_kg') + " kg/anode", "ASSUMED - to be confirmed", "The paper prints nominal mass. Core deductions are not established."],
        ["Connected wells", n('inputs.structure.connected_current_allowances.0.count', 0) + " × " + n('inputs.structure.connected_current_allowances.0.current_A_each') + " A", "ASSUMED - to be confirmed", "The published allowance is applied to every phase; the source does not split phases."],
        ["Installed allocation by zone", "NOT_EVALUATED", "Required input", "Member positions, continuity and anode allocation are absent from the example paper."],
        ["Splash, conductors and installed pile dimensions", "NOT_EVALUATED", "Required input", "Do not infer dimensions from aggregate area."],
        ["Retrofit date and spatial count", "NOT_EVALUATED", "Required input", "Survey date, depletion, continuity and local current distribution are required."],
    ]
    comparison_rows = []
    labels = {"initial_demand": ("Initial demand", "A"), "mean_demand": ("Mean demand", "A"),
              "final_demand": ("Final demand", "A"), "initial_output": ("Fresh individual output", "A"),
              "final_output": ("Depleted individual output", "A"), "count": ("Governing count", "anodes")}
    for name, (label, unit) in labels.items():
        prefix = 'comparison.'+name+'.'
        places = 0 if name == 'count' else 1
        comparison_rows.append([label, n(prefix+'reference', places), n(prefix+'calculated', places),
                                n(prefix+'difference_percent') + ' %', unit, 'Public design record; ' + src])
    sections: dict[str, str] = {}
    sections['summary'] = f'''<p>The cathodic protection design module is a calculation library for sacrificial-anode demand, mass and current output.
Analysis is performed to determine the aggregate anode requirement for a published jacket example; key results are summarized below.</p>
<ul><li>The aggregate requirement is read as {n(v+'recommended_anode_count', 0)} anodes on the declared assumption basis.
<ul><li>The published {n(v+'anode_count', 0)}-anode arrangement fails the modelled initial-current requirement; an increase to {n(v+'recommended_anode_count', 0)} anodes is recalculated and passes the aggregate checks.</li>
<li>Field protection remains NOT_EVALUATED by this calculation. A distributed potential survey and local allocation assessment are required before a retrofit is specified.</li></ul></li>
<li>The scope of evidence is identified before use.<ul><li>Published phase demands are reproduced to their printed precision; the equivalent-cylinder output differs from the paper.</li>
<li>Design life, net alloy interpretation and material assumptions require confirmation. The calculation is not an installed-design acceptance.</li></ul></li></ul>
<h3>Revision history</h3>{table(['Revision', 'Date', 'Change and cause'], [['0', DATE, 'Initial module analysis; public arithmetic evidence and engineering limits are separated.']])}'''
    sections['scope'] = '''<p>The main offshore sacrificial-anode workflow is described from zone definition through a governing anode count.
The calculation is bounded to an electrically connected structure with declared current demands and anode geometry.</p>
<p>Seawater and buried-steel areas are grouped by exposure, coating and depth. Initial current is used to establish polarisation capacity;
mean current is used to size consumable alloy; final current is checked against the depleted anode geometry.
A sufficient total mass alone does not establish sufficient initial or final current.</p>
<p>The engineering inputs are areas, exposure definitions, coating condition or effective phase densities, connected loads,
design life, electrolyte resistivity, alloy capacity and utilisation, anode dimensions, net alloy mass and provided count.
The outputs are phase demands, required mass, fresh and depleted resistance and output, separate governing counts,
a recommended total and aggregate pass or fail checks. The library returns calculation data; report presentation is supplied separately.</p>'''
    sections['basis'] = f'''<p>The published example is evaluated on its own area and effective current-density basis.
The original standard edition is not identified in the paper; the module is explicitly evaluated with the May 2021 edition selector.</p>
<p>The published effective densities already represent each surface category. A unity coating multiplier is therefore applied;
a second coating deterioration factor is excluded to avoid counting coating effects twice. The connected-well allowance is applied at each phase.
This phase interpretation is an assumption, recorded in the appendix.</p>
<p>For an aggregate pass, provided alloy mass is at least the mean-demand mass requirement, fresh total output is at least initial demand,
and depleted total output is at least final demand. Each criterion is necessary because charge capacity and instantaneous output constrain different failure modes.
The corresponding standard references are DNV-RP-B401, Cathodic protection design, May 2021: the anode material design values,
anode resistance and utilisation tables and the design protective-potential clause. Numeric standard tables are not reproduced.</p>
<p>The active capacity and utilisation are checked against the existing example's declared assumptions before this report is written.
Their applicability is not established by that equality check. Source editions, equation locators and assumption provenance are listed in the {src}.</p>'''
    sections['theory'] = f'''<p>Current demand and consumed charge are evaluated before the current-output constraint is applied.
The equations below describe the implemented main workflow; edition-specific applicability remains subject to engineering review.</p>
<div class="formula">Iₚ = Σ(A𝓏 i𝓏,ₚ f𝓏,ₚ) + Iconnected<br>
M = Imean t H / (ε u)<br>
f(t) = min(a + bt, unity)<br>
fmean = min(a + bt/2, unity)<br>
r = √(m / (π L ρalloy)); rfinal = √((unity − u)m / (π L ρalloy))<br>
R = ρwater [ln(4L/r) − 1] / (2πL), for L ≥ 4r<br>
Ianode = ΔE / R<br>
N = max(ceil(M/m), ceil(Iinitial/Ianode,initial), ceil(Ifinal/Ianode,final))</div>
<p>Phase p is initial, mean or final, and z denotes a surface zone. Area A is in square metres; density i is in amperes per square metre.
It is a bare-steel design density, or an effective density when f is unity;
f is the dimensionless coating multiplier. Current I is in amperes. Required net alloy mass M and individual mass m are in kilograms;
life t is in years; H is the hours-per-year conversion used by the library; ε is electrochemical capacity in ampere hours per kilogram;
u is the usable alloy fraction.</p>
<p>Length L and equivalent radius r are in metres; electrolyte resistivity ρwater is in ohm metres and alloy density ρalloy is in kilograms per cubic metre.
Resistance R is in ohms and driving voltage ΔE is in volts. The ceiling operator rounds upward to an integer anode count N.
The resistance expression is the long slender stand-off form; the module selects a different form outside its length-to-radius domain.
Fresh and depleted geometry are evaluated separately at fixed length.</p>
<p>For the general linear coating model, a is the initial bare fraction and b is its annual increase. The library's mean factor is the capped midpoint value,
not the time integral of a curve that saturates during the design life. A qualified coating model or explicit phase inputs are required for that condition.
In this worked example, phase-specific effective densities are used directly.
All equation and conversion records are identified in the {src}.</p>'''
    sections['worked-example'] = f'''<p>The public jacket example is calculated by calling the production module for the published arrangement and for the increased arrangement.
Only the documented phase-density override is applied; the material-default equality check is retained.</p>
<p>The example mudline is at {n('inputs.structure.report_geometry.mudline_m')} m relative to still-water level; member coordinates are not supplied.
The reported installed arrangement contains {n(v+'anode_count', 0)} anodes of nominal mass {n('inputs.anode.individual_anode_mass_kg')} kg
and length {n('inputs.anode.length_m', 3)} m. Seawater resistivity is {n('inputs.environment.seawater_resistivity_ohm_m', 2)} Ω·m;
the module driving voltage is {n(v+'driving_voltage_V', 2)} V. Nominal mass is interpreted as net alloy for the calculation.</p>
<p>The module returns initial, mean and final total demands of {n(d+'total_initial_A')}, {n(d+'total_mean_A')} and {n(d+'total_final_A')} A.
The net alloy requirement is {n(r+'total_mass_kg')} kg against {n('derived.provided_mass_kg')} kg when nominal mass is treated as net alloy.
The mass, initial-current and final-current counts are respectively {n(v+'count_by_mass', 0)}, {n(v+'count_by_initial_current', 0)}
and {n(v+'count_by_final_current', 0)} anodes.</p>
<p>The fresh and depleted individual outputs are {n(v+'anode_current_output_initial_A', 2)} A and {n(v+'anode_current_output_final_A', 2)} A.
The supplied arrangement provides {n(v+'total_anode_current_output_initial_A')} A initially, below demand of {n(v+'initial_current_demand_A')} A.
The initial-current requirement governs. Increasing the total to {n(v+'recommended_anode_count', 0)} anodes is recalculated by the same module;
the increased arrangement provides {n('rechecked.current_output_verification.total_anode_current_output_initial_A')} A initially and passes all aggregate checks.</p>
<p>The area, density, individual resistance and source-comparison records are retained in the <a href="#data-register">data register</a>.
The public paper, its tables and the exact reproduction entry point are identified in the <a href="#sources">source register</a>.</p>'''
    sections['results'] = '''<p>The governing initial-current shortfall is shown against the supplied anode-count limit.
Mass and final output pass for the stated assumptions; the initial output passes only after the aggregate count is increased.</p>
''' + count_chart(values) + '''
<p>Each zone contributes to the initial, mean and final demand listed in the appendix. Anodes are shared across the connected structure in this calculation;
required and provided anode mass and count per zone are NOT_EVALUATED because anode allocation and the connected-load split are not supplied.
An aggregate pass does not establish a local protection margin.</p>
<p>The repository holds independent arithmetic tests of current demand, mass, resistance and selection of the largest criterion count,
as well as edition and input-boundary checks. The public jacket tests separately check source transcription, phase-demand reproduction,
geometry-derived outputs, explicit assumptions and failing and increased aggregate arrangements.</p>
<p>Published demands are reproduced to their printed precision. Individual output and the governing count differ from the paper because the equivalent-cylinder model
and the material and life assumptions do not establish the original design method. Percentage differences are recorded against the published reference values in the appendix.
Agreement of functions that share a formula kernel is consistency evidence; it is not an independent field validation.</p>'''
    sections['limits'] = '''<p>Aggregate current and alloy charge capacity are evaluated; spatial potential distribution is not evaluated by this workflow.
Release of a final design is withheld until the missing design and survey evidence is resolved by the responsible engineer.</p>
<p>The module's use status requires a responsible engineer to check each deliverable. The standard edition and shape applicability require a separate design-basis review.</p>
<p>The published cross-section does not establish core deductions or the actual net alloy volume. The equivalent cylinder is an approximation at fixed length;
product geometry, anode-to-steel separation, shielding, electrical continuity, sediment current paths and local depletion are not resolved here.
The paper reports lower-level under-protection despite the original aggregate design, illustrating the spatial limitation.</p>
<h3>Not covered</h3><p>Pipeline bracelet and terminal-bank design, ship-hull calculations, multiple anode families, component continuity and phased depletion are separate capabilities
in the module. Impressed-current supply and cable design, surveys, monitoring, stray-current models and corrosion-rate estimates are also outside this analysis.
Deprecated and experimental functions require their own use-status and source review. No comparison with a private historical design is included.</p>'''
    sections['recommendations'] = f'''<p>The initial-current shortfall is resolved in the aggregate model by an increased anode count.
A field retrofit remains conditional on local demand, continuity, condition and potential evidence.</p>
<p>At design, increase the stated arrangement by {n('derived.additional_anodes', 0)} anodes to {n(v+'recommended_anode_count', 0)} anodes of the assumed net mass and geometry.
This total is recalculated and passes the aggregate mass and current criteria. The calculation basis is the module implementation of the
May 2021 resistance, material and utilisation definitions listed in the source register. Spatial layout remains NOT_EVALUATED.</p>
<p>In service, obtain potentials, continuity and anode-condition evidence at the lower levels and the buried-steel connections before a retrofit is sized.
Under-protection at a location is the trigger for a local demand and distribution assessment. A numeric potential trigger is a required design input for the
selected reference electrode and service conditions; it is NOT_EVALUATED here. The public paper's remedy places additional sacrificial anodes at the affected
lower levels; completion and post-retrofit performance are not established by the supplied evidence.</p>
<p>Confirm design life, alloy capacity and utilisation, nominal-to-net mass and anode geometry before the aggregate count is released.
Retrofit quantities by level and a completion date are NOT_EVALUATED. The public remedy and the calculation criteria are located in the <a href="#sources">source register</a>.</p>'''
    sections['appendix'] = f'''<p>The evidence and missing-input registers are retained with the calculation.
Source locators and reproduction instructions are separated from the main engineering narrative.</p>
<h3 id="data-register">9.1 Data register and quality evidence</h3>
<p>Table A. Phase-density inputs are effective densities; no further coating reduction is applied. The coating multiplier is unity by definition for this input convention.</p>
{table(['Zone', 'Area (m²)', 'Initial (mA/m²)', 'Mean (mA/m²)', 'Final (mA/m²)', 'Source'], zone_rows)}
<p>Table B. Zone demand is returned by the module. The connected allowance is added to the totals, outside the zone rows.</p>
{table(['Zone', 'Initial (A)', 'Mean (A)', 'Final (A)', 'Source'], demand_rows)}
<p>Table C. Assumptions are inherited from the public repository example, assumed, based on previous experience, and labelled ASSUMED - to be confirmed.
They are not measurements or newly sourced alloy qualifications.</p>
{table(['Item', 'Value or status', 'Basis status', 'Evidence or missing input'], basis_rows)}
<p>Table D. The comparison uses the published value as reference. Differences are calculated before rounding and shown as absolute percentage differences.
A nonzero difference below the reporting resolution is bounded rather than printed as zero. These are source comparisons, not acceptance tolerances.</p>
{table(['Quantity', 'Published', 'Calculated', 'Absolute difference', 'Unit', 'Source'], comparison_rows)}
<p>Fresh equivalent radius is {n(v+'equivalent_radius_initial_m', 3)} m and depleted equivalent radius is {n(v+'equivalent_radius_final_m', 3)} m.
Fresh and depleted resistance are {n(v+'anode_resistance_initial_ohm', 3)} Ω and {n(v+'anode_resistance_final_ohm', 3)} Ω.
The anode length is held fixed. The hour conversion is the library constant, not a site-specific availability allowance.</p>
<p>Table E. Quality evidence is bounded to the recorded numerical and document checks.</p>
{table(['Pre-flight item', 'Evidence and limit'], [
['Numerical origin', 'Each annotated quantity is bound to the module result or the documented input. Recalculation and deterministic rebuild are checked by the accompanying test.'],
['Assumption drift', 'Declared material assumptions are compared with active defaults; a mismatch stops report generation.'],
['Acceptance logic', 'The supplied aggregate arrangement fails initial current. The increased arrangement is calculated again and its aggregate pass is required.'],
['Evidence independence', 'Source transcription and independent hand arithmetic are distinct from agreement among shared-kernel functions.'],
['Client text', 'Built-page headings, links, number annotations, terminology, local assets and count-limit annotations are checked. Comment save, load and stale-file rejection are tested at script level; browser visual review and native file-dialog behaviour are not recorded.'],
['Source access', 'Both open papers were read on '+DATE+'; each publisher returned HTTP 200. Licensed standards were not accessed for this analysis.'],
['Comments and changes', 'The change register is embedded. Comments are retained locally and exported to a matching document file; no hosted comment service is used.'],
])}
<h3 id="sources">9.2 Sources and reproduction locators</h3>
<ol><li><a href="{PAPER}">Study on Implementation and Optimization of Cathodic Protection for Offshore Oil Platform Jacket</a>,
Engineering Advances, 2023, volume 3, issue 2, pages 241–245, DOI 10.26855/ea.2023.06.017.
Tables 1–2 and the design paragraph on page 242 supply areas, effective densities, connected allowance, dimensions, nominal mass, driving voltage,
resistivity, count and comparison outputs. Sections 3–5 supply the observed spatial limitation and proposed lower-level remedy.
The paper does not identify the standard edition or establish the assumed life and alloy properties.</li>
<li>The existing <a href="../../../../tests/fixtures/cathodic_protection/workflow_inputs/published_jacket_84m.yml">public-example input record</a>
contains the transcription, source locators and assumption statement. Its design-data, environment, anode and structure sections are read without copied result values.</li>
<li>DNV-RP-B401, Cathodic protection design, May 2021: design protective potential, clause 2.4.2; anode design values, Table 8-6;
anode resistance, Table 8-7; utilisation factors, Table 8-8. The original licensed text is not supplied or independently inspected here.
The <a href="../../../../src/digitalmodel/cathodic_protection/b401_tables.py">edition-keyed source mapping</a> identifies these references.
Repository wrappers retain some older-edition equation numbering; the material assumption check does not resolve normative applicability.</li>
<li>The <a href="../../../../src/digitalmodel/cathodic_protection/_kernels.py">formula kernel</a> supplies current demand, charge balance,
coating factors, equivalent radii, resistance and hours-per-year conversion.
The <a href="../../../../src/digitalmodel/cathodic_protection/engine_adapter.py">production entry point</a> supplies the zone and maximum-count workflow.
The <a href="../../../../src/digitalmodel/cathodic_protection/anode_sizing.py">anode-sizing implementation</a> supplies the fixed-length depletion and resistance selection.</li>
<li><a href="{GEOMETRY_PAPER}">Design optimization of offshore wind jacket piles by assessing support structure orientation relative to metocean conditions</a>,
Wind Energy Science, 2023, volume 8, pages 807–817, sections 3.2–3.3, Figure 3 and Table 4, CC BY 4.0.
The separate <a href="../../../../tests/fixtures/cathodic_protection/workflow_inputs/reference_jacket_60m_members.yml">member schedule</a>
contains the openly licensed model transcription. Attribution and Apache License 2.0 terms are retained in its
<a href="../../../../tests/fixtures/cathodic_protection/workflow_inputs/reference_jacket_60m.LICENSE.txt">licence record</a>.
Figure A is drawn from coordinates, not copied from the paper. Joint offsets, end caps and intersections are not resolved.</li>
<li>The existing tests cover <a href="../../../../tests/cathodic_protection/test_kernels.py">shared arithmetic</a>,
<a href="../../../../tests/cathodic_protection/test_anode_sizing.py">governing sizing</a>,
<a href="../../../../tests/cathodic_protection/test_engine_adapter.py">entry-point behaviour</a>,
<a href="../../../../tests/cathodic_protection/test_report_adapters.py">published jacket evidence</a>,
<a href="../../../../tests/cathodic_protection/test_reference_jacket.py">member transcription</a> and
<a href="../../../../tests/cathodic_protection/test_worked_examples.py">documented executable examples</a>.
No collection count is presented as an executed-test result.</li>
<li>The report uses the shared <a href="../../../../src/digitalmodel/reporting/assets/calc_report_format.html">calculation-report stylesheet</a>,
also used by the <a href="../../../domains/cathodic_protection/reports/reference-jacket/cathodic-protection-analysis.html">existing jacket report</a>.
The stylesheet is inlined with the document body class required for report delivery.
Rebuild with <code>PYTHONPATH=src python scripts/reporting/build_cp_module_report.py --output &lt;report-directory&gt;</code>.
The <a href="../../../../tests/reporting/test_cp_module_report.py">report contract test</a> rebuilds into a temporary directory,
checks every annotated value against a fresh production call, and compares the delivered files byte for byte.
The builder requires the module's Python dependencies and the repository fixtures; it makes no network request.</li></ol>'''
    sections['glossary'] = '''<p>The symbols used in the equations are defined with their units.
Phase suffixes identify the demand and anode condition used in each calculation.</p>
<p>Cathodic protection is the reduction of corrosion by shifting the steel potential through externally supplied current.
A sacrificial anode supplies that current by consuming alloy. A stand-off anode is separated from the protected steel by its supports.</p>
<p>A: steel area (m²); i: phase current density (A/m²); f: coating breakdown fraction; I: current (A);
M: total required net alloy mass (kg); m: individual net alloy mass (kg); t: design life (years); H: hours per year;
ε: alloy capacity (Ah/kg); u: utilisation fraction; L: anode length (m); r: equivalent radius (m);
ρwater: electrolyte resistivity (Ω·m); ρalloy: alloy density (kg/m³); R: anode resistance (Ω);
ΔE: driving voltage (V); N: anode count; a: initial coating breakdown; b: annual breakdown rate (per year).
Initial means fresh-anode polarisation demand, mean means life-average demand and final means end-of-life demand and depleted geometry.</p>
<p>The count chart uses anodes. Current-density conversions between amperes and milliamperes use the metric-prefix definition.
A pass means only that the stated aggregate inequality is satisfied. A fail means that it is not satisfied.
NOT_EVALUATED means that the required evidence is absent. An assumption marked ASSUMED - to be confirmed remains an input requiring engineering confirmation.</p>'''
    # Keep the house stylesheet byte-for-byte rather than introducing a second
    # report theme. Component rules below only support the new local controls.
    style_match = re.search(r'<style>.*?</style>', STYLE.read_text(encoding="utf-8"), re.S)
    if style_match is None:
        raise ValueError("The shared calculation-report stylesheet is missing")
    style = style_match[0]
    material = (Path(__file__).read_bytes() + STYLE.read_bytes() + EXAMPLE.read_bytes()
                + GEOMETRY.read_bytes() + json.dumps(values, sort_keys=True).encode())
    fingerprint = hashlib.sha256(material).hexdigest()
    toc = ' · '.join(f'<a href="#{key}">{title}</a>' for key, title in SECTIONS)
    body = ''.join(f'<section id="{key}"><h2>{title}</h2>{sections[key]}</section>' for key, title in SECTIONS)
    options = ''.join(f'<option value="{key}">{title}</option>' for key, title in SECTIONS)
    return f'''<!doctype html>
<html lang="en" data-theme="light"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">
<title>Cathodic protection module analysis</title><meta id="document-key" name="document-key" content="{fingerprint}">
{style}
<style>
.hs-doc h3{{margin:1.4em 0 .6em}} .hs-doc svg{{max-width:100%;height:auto}}
.hs-doc .tbl{{overflow-x:auto}} .hs-doc td,.hs-doc th{{white-space:normal}}
.hs-doc textarea{{display:block;width:100%;min-height:6em}} .hs-doc button,.hs-doc select,.hs-doc input{{font:inherit;margin:.4em;padding:.4em}}
.hs-doc :focus-visible{{outline:3px solid var(--cfd);outline-offset:3px}}
.hs-doc .limit{{pointer-events:none}} .hs-doc figure{{margin:1.5em 0}} .hs-doc figcaption{{color:var(--ink-2)}}
@media print{{.review-controls,nav{{display:none}} .hs-doc section{{break-before:auto}}}}
</style></head><body class="doc hs-doc">
<header class="hero"><div class="wrap"><p class="eyebrow">Cathodic protection</p><h1>Cathodic protection module analysis</h1><p>Draft for client review</p></div></header>
<div class="wrap"><nav aria-label="Contents">{toc}</nav><main>{body[:body.index('<section id="theory">')]}{geometry_figure(values)}{body[body.index('<section id="theory">'):]}</main>
<aside class="card review-controls" aria-label="Changes and comments"><details><summary>Engineering changes</summary><ul id="change-list"></ul></details>
<h3>Report comments</h3><p>Select report text and add a comment. The section and selected quotation are retained with the note.
Save locally saved comments beside this HTML as <code>comments.json</code>; load that file to continue. The file is matched to this report's content.
Browser storage is a working copy; the saved comment file is the retained record.</p>
<label for="comment-section">Section</label><select id="comment-section">{options}</select>
<label for="comment-text">Comment</label><textarea id="comment-text" maxlength="10000"></textarea>
<button id="add-comment" type="button">Add comment</button><button id="save-comments" type="button">Save comments beside report</button>
<label>Load comments <input id="load-comments" type="file" accept="application/json"></label>
<p id="comment-status" role="status">Comments remain local to this browser until saved.</p><ol id="comment-list"></ol></aside></div>
<script id="calculation-data" type="application/json">{json.dumps(values, sort_keys=True)}</script>
<script id="changes-data" type="application/json">{json.dumps(CHANGES, sort_keys=True)}</script>
<script>{UI_SCRIPT}</script></body></html>
'''


def build(output: Path = OUTPUT) -> None:
    """Write the HTML and matching engineering-change sidecar."""
    html = render(calculate())
    output.mkdir(parents=True, exist_ok=True)
    (output / "index.html").write_text(html, encoding="utf-8")
    (output / "changes.json").write_text(json.dumps(CHANGES, indent=2) + "\n", encoding="utf-8")


def main() -> None:
    """Parse the optional output directory and rebuild the analysis."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, default=OUTPUT)
    args = parser.parse_args()
    build(args.output)


if __name__ == "__main__":
    main()
