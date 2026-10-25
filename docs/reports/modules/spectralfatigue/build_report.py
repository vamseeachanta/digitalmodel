# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.
"""Rebuild the self-contained spectral fatigue module report from public inputs.

Example: PYTHONPATH=src python docs/reports/modules/spectralfatigue/build_report.py
    --work-dir "$TMPDIR/spectral-calculation"
Only the HTML and change panel are written to the report destination.
"""

from __future__ import annotations

import argparse
import ast
import copy
import csv
import html
import json
import math
import re
from pathlib import Path

import yaml

from digitalmodel.fatigue.spectral_fatigue import compute_spectral_moments, dirlik_damage
from digitalmodel.spectral_fatigue.workflow import router

ROOT = Path(__file__).resolve().parents[4]
HERE = Path(__file__).resolve().parent
EXAMPLE = ROOT / "examples/workflows/spectral-fatigue/input.yml"
EXEMPLAR = ROOT / "docs/reports/pipeline-ffs-assessment.html"
REFERENCE = "https://mdpi-res.com/d_attachment/metals/metals-11-01333/article_deploy/metals-11-01333.xml"
SECTIONS = (
    ("executive-summary", "Executive Summary"),
    ("introduction", "Introduction"),
    ("design-data", "Design Data"),
    ("methodology", "Analysis Methodology"),
    ("detailed-results", "Detailed Results"),
    ("findings", "Key Findings"),
    ("limitations", "Limitations"),
    ("recommendations", "Recommendations"),
    ("references", "References"),
    ("acronyms", "Acronyms and Symbols"),
)
LABELS = {"riser-splash-zone": "Splash-zone detail", "riser-keel-joint": "Keel joint"}
CHANGES = {
    "report": "Spectral fatigue module analysis",
    "revisions": [{
        "revision": "1", "date": "2026-10-25",
        "changes": [
            "Saved comments for matching report content are validated and merged to preserve earlier feedback.",
            "Hand-calculated moment, damage and life checks distinguish reproduction from reference damage.",
            "Rebuild comparisons allow numerical roundoff; text output uses explicit UTF-8 and LF.",
        ],
    }, {
        "revision": "0", "date": "2026-10-21",
        "changes": [
            "First issue: public example results are reproduced from the spectral fatigue workflow.",
            "Frequency convention and estimator qualification gaps are identified before design use.",
            "Input, evidence and recommendation registers are provided for engineering review.",
        ],
    }],
}


def lookup(record: dict, locator: str):
    """Resolve a numeric binding within the calculation record."""
    value = record
    for part in locator.split("."):
        value = value[int(part)] if isinstance(value, list) else value[part]
    return value


def format_number(value: float, kind: str = "decimal") -> str:
    """Use engineering precision without displaying nonzero values as zero."""
    if kind == "integer":
        text = str(int(value))
    elif kind == "input":
        text = str(value)
    elif kind == "hundredths":
        text = f"{value:.2f}"
    elif kind == "scientific" or (value != 0 and abs(value) < 0.05):
        text = f"{value:.1e}"
    else:
        text = f"{value:.1f}"
    return text.replace("-", "−")


def number(record: dict, locator: str, kind: str = "decimal") -> str:
    """Bind the displayed value and full precision to the same module record."""
    value = lookup(record, locator)
    return (f'<span data-number="{locator}" data-value="{value}" data-format="{kind}">'
            f'{format_number(value, kind)}</span>')


def definition_count(path: str) -> int:
    """Count test definitions rather than claiming independent benchmark coverage."""
    tree = ast.parse((ROOT / path).read_text(encoding="utf-8"))
    return sum(isinstance(node, ast.FunctionDef) and node.name.startswith("test_")
               for node in ast.walk(tree))


def calculate(work_dir: Path) -> dict:
    """Call the public workflow; keep every CSV and temporary file outside docs."""
    work_dir = work_dir.resolve()
    if work_dir == ROOT or ROOT in work_dir.parents:
        raise ValueError("Calculation directory must be outside the repository")
    source = yaml.safe_load(EXAMPLE.read_text(encoding="utf-8"))
    settings = copy.deepcopy(source["spectral_fatigue"])
    cfg = copy.deepcopy(source)
    cfg["spectral_fatigue"]["output_dir"] = str(work_dir / "csv")
    result = router(cfg)["spectral_fatigue"]
    with Path(result["results_csv"]).open(encoding="utf-8") as stream:
        rows = list(csv.DictReader(stream))
    states = []
    for li, location in enumerate(settings["locations"]):
        for si, sea_state in enumerate(location["sea_states"]):
            moments = compute_spectral_moments(sea_state["frequency_Hz"], sea_state["stress_psd_MPa2_Hz"])
            fatigue = dirlik_damage(moments, settings["sn_curve"]["slope"], settings["sn_curve"]["log_intercept"])
            hertz_peak = math.sqrt(moments.m4 / moments.m2)
            states.append({
                "location_index": li, "state_index": si,
                "label": LABELS[location["id"]],
                "moments": moments.model_dump(), "result": fatigue.model_dump(),
                "rms": moments.rms, "alpha2": moments.irregularity_factor,
                "epsilon": moments.bandwidth_parameter,
                "peak_rate": moments.nu_p, "crossing_rate": moments.nu_0,
                "hertz_peak_rate": hertz_peak,
                "peak_difference_percent": 100 * (moments.nu_p / hertz_peak - 1),
            })
    return {"input": settings, "summary": result["locations"], "sea_states": rows,
            "states": states, "screening_status": result["screening_status"],
            "governing_location": result["governing_location"],
            "evidence": {"estimator_test_definitions": definition_count("tests/fatigue/test_spectral_fatigue.py")}}


def house_style() -> str:
    """Reuse the existing ten-section engineering report stylesheet verbatim."""
    return re.search(r"<style>.*?</style>", EXEMPLAR.read_text(encoding="utf-8"), re.S)[0]


def table(headers: list[str], rows: list[list[str]], caption: str) -> str:
    """Render actual columns with an explicit source column at each call site."""
    head = "".join(f'<th scope="col">{html.escape(v)}</th>' for v in headers)
    body = "".join("<tr>" + "".join(f"<td>{v}</td>" for v in row) + "</tr>" for row in rows)
    return f'<div class="table-wrap"><table><caption>{caption}</caption><thead><tr>{head}</tr></thead><tbody>{body}</tbody></table></div>'


def section(key: str, text: str) -> str:
    """Place each chapter inside the standard ten-section frame."""
    index, title = next((i, t) for i, (k, t) in enumerate(SECTIONS, 1) if k == key)
    return f'<section id="{key}"><h2>{index}. {title}</h2>{text}</section>'


def encode(value) -> str:
    """Embed JSON without allowing markup to terminate its script element."""
    return json.dumps(value, ensure_ascii=True, allow_nan=False).replace("<", "\\u003c")


def chart_data(record: dict) -> list[dict]:
    """Plot returned life against design life assessed and the growing requirement."""
    years = record["input"]["design_life_years"]
    dff = record["input"]["dff"]
    return [{"label": LABELS[row["location_id"]], "limit": row["required_life_years"],
             "points": [[0.0, row["fatigue_life_years"]], [years, row["fatigue_life_years"]]],
             "limit_points": [[0.0, 0.0], [years, years * dff]]}
            for row in record["summary"]]


def schematic() -> str:
    """Show the physical interpretation while naming unavailable geometry."""
    return '''<figure><svg viewBox="0 0 840 360" role="img" aria-labelledby="system-title system-desc">
<title id="system-title">Riser detail stress-spectrum assessment</title>
<desc id="system-desc">A vertical riser passes a floating support at the waterline. Splash-zone and keel details are shown beside the spectral model. Geometry, supports and seabed conditions are not supplied.</desc>
<rect x="15" y="10" width="810" height="335" fill="white" stroke="var(--line)"/>
<path d="M35 135 Q65 120 95 135 T155 135 T215 135 T275 135 T335 135" fill="none" stroke="var(--report-accent)"/>
<path d="M75 65 L285 65 L260 115 L100 115 Z" fill="var(--line)" stroke="var(--ink)"/>
<path d="M175 45 L175 300 M190 45 L190 300" fill="none" stroke="var(--ink)" stroke-width="5"/>
<circle cx="182" cy="137" r="8" fill="var(--report-accent)"/><circle cx="182" cy="192" r="8" fill="var(--report-accent)"/>
<path d="M195 137 L320 157 M195 192 L320 205" fill="none" stroke="var(--ink)"/>
<text x="325" y="160">Splash-zone detail</text><text x="325" y="208">Keel joint</text>
<text x="35" y="40">Floating support: geometry unavailable</text>
<text x="35" y="327">Lower termination and seabed: unspecified</text>
<text x="510" y="55">Model boundary</text><text x="510" y="95">Supplied local stress spectra</text>
<path d="M635 108 L635 142" stroke="var(--ink)"/>
<text x="510" y="165">Spectral moments and estimator</text>
<path d="M635 180 L635 217" stroke="var(--ink)"/>
<text x="510" y="240">Occurrence-weighted annual damage</text>
<text x="510" y="282">No structural mesh or motion solution</text>
</svg><figcaption>Figure A. Physical interpretation of the public example, not a dimensioned riser design. Support restraint, depth, diameter and stress-transfer derivation are absent. The model begins at the supplied local stress spectrum.</figcaption></figure>'''


CHART_SCRIPT = r'''
(() => {
  const series = JSON.parse(document.getElementById('chart-data').textContent);
  const ns = 'http://www.w3.org/2000/svg';
  const fmt = v => v !== 0 && Math.abs(v) < .05 ? v.toExponential(1) : v.toFixed(1);
  const make = (tag, attrs, text) => {
    const e = document.createElementNS(ns, tag);
    Object.entries(attrs).forEach(([k,v]) => e.setAttribute(k,v));
    if (text !== undefined) e.textContent = text;
    return e;
  };
  series.forEach((s, index) => {
    const svg = document.getElementById(`life-chart-${index}`);
    const xMax = s.points.at(-1)[0], yMax = Math.max(s.limit, s.points[0][1]) * 1.15;
    const x = v => 75 + v / xMax * 650, y = v => 270 - v / yMax * 225;
    svg.append(make('path', {d:'M75 40 V270 H740',fill:'none',stroke:'var(--ink)'}));
    for (const value of [0, xMax / 2, xMax])
      svg.append(make('text',{x:x(value),y:290,'text-anchor':'middle'},fmt(value)));
    for (const value of [0, yMax / 2, yMax])
      svg.append(make('text',{x:65,y:y(value),'text-anchor':'end'},fmt(value)));
    svg.append(make('text',{x:400,y:320,'text-anchor':'middle'},'Design life assessed (years)'));
    svg.append(make('text',{x:20,y:155,transform:'rotate(-90 20 155)','text-anchor':'middle'},'Life (years)'));
    const line = (points, color, dashed) => {
      const e=make('polyline',{points:points.map(p=>`${x(p[0])},${y(p[1])}`).join(' '),
        fill:'none',stroke:color,'stroke-width':3});
      if (dashed) {e.setAttribute('stroke-dasharray','8 5');e.setAttribute('data-limit','required-life');}
      svg.append(e);
    };
    line(s.points,'var(--ink)',false);
    line(s.limit_points,'var(--report-accent)',true);
    const status = document.getElementById(`life-hover-${index}`);
    const detail = year => `Case: public example; component: ${s.label}; design life assessed: ${fmt(year)} years; demand: required life ${fmt(year*s.limit/xMax)} years; limit: calculated available life ${fmt(s.points[0][1])} years. Screening only.`;
    const announce = year => { status.textContent=detail(year); };
    for (const point of [...s.points, ...s.limit_points]) {
      const dot=make('circle',{cx:x(point[0]),cy:y(point[1]),r:6,fill:'var(--ink)',tabindex:0,'aria-label':detail(point[0])});
      dot.append(make('title',{},detail(point[0])));
      dot.addEventListener('focus',()=>announce(point[0]));
      dot.addEventListener('pointerenter',()=>announce(point[0]));
      svg.append(dot);
    }
    svg.addEventListener('pointermove',event=>{
      const point=svg.createSVGPoint();point.x=event.clientX;point.y=event.clientY;
      const local=point.matrixTransform(svg.getScreenCTM().inverse());
      announce(Math.max(0,Math.min(xMax,(local.x-75)/650*xMax)));
    });
    announce(xMax);
  });
})();
'''

COMMENT_SCRIPT = r'''
(() => {
  const id = 'spectral-fatigue-module-analysis';
  const revision = JSON.parse(document.getElementById('change-data').textContent).revisions[0].revision;
  const el = id => document.getElementById(id);
  const sections = [...document.querySelectorAll('main section[id]')];
  const allowed = new Set(sections.map(s=>s.id));
  let comments = [], directory = null, digest = null, loadedDigest = null, loadedRecord = {};
  const say = text => {el('comment-status').textContent=text;};
  const show = () => {el('comment-preview').textContent=comments.map(c=>`${c.section}: ${c.text}`).join('\n');};
  const hash = async bytes => [...new Uint8Array(await crypto.subtle.digest('SHA-256', bytes))].map(b=>b.toString(16).padStart(2,'0')).join('');
  const validate = (value, expectedDigest) => {
    if(!value||value.report!==id||value.revision!==revision||value.calculation!==el('report-data').textContent||
      !Array.isArray(value.comments)||value.comments.some(c=>!c||!allowed.has(c.section)||typeof c.text!=='string'||!c.text.trim())||
      !(value.report_digest===null||(typeof value.report_digest==='string'&&/^[a-f0-9]{64}$/.test(value.report_digest))))
      throw Error('Comment file does not match this report. Retain the original under a different filename before saving a new comment file; keep other revisions separately.');
    if(expectedDigest&&value.report_digest!==expectedDigest)
      throw Error('Saved comments have no matching report fingerprint. Retain the original under a new filename; load matching comments before selecting the folder again.');
    return value.comments;
  };
  const merge = (retained, incoming) => {
    const combined=[...retained], seen=new Set(retained.map(c=>JSON.stringify([c.section,c.text])));
    incoming.forEach(c=>{const key=JSON.stringify([c.section,c.text]);if(!seen.has(key)){combined.push(c);seen.add(key);}});
    return combined;
  };
  sections.forEach(s=>{const o=document.createElement('option');o.value=s.id;o.textContent=s.querySelector('h2,h3').textContent;el('comment-section').append(o);});
  el('comment-folder').onclick=async()=>{
    try {
      directory=null;digest=null;
      if(!window.showDirectoryPicker) return say('Folder access is unavailable. Load existing comments before downloading; save under a new filename to preserve the original.');
      const chosen=await window.showDirectoryPicker({mode:'readwrite'});
      const file=await (await chosen.getFileHandle('report.html')).getFile();
      const bytes=await file.arrayBuffer();
      const parsed=new DOMParser().parseFromString(new TextDecoder().decode(bytes),'text/html');
      if(parsed.getElementById('report-data')?.textContent!==el('report-data').textContent)
        throw Error('The selected folder contains a different calculation.');
      if(parsed.getElementById('change-data')?.textContent!==el('change-data').textContent)
        throw Error('The selected folder contains a different report revision.');
      const selectedDigest=await hash(bytes);
      if(loadedDigest&&loadedDigest!==selectedDigest) throw Error('Loaded comments belong to different report content.');
      digest=selectedDigest;directory=chosen;
      say('Matching report folder selected. Save writes comments beside report.html.');
    }catch(error){say(error.message);}
  };
  el('comment-add').onclick=()=>{
    const text=el('comment-text').value.trim();
    if(!text) return say('Enter a comment.');
    comments.push({section:el('comment-section').value,text});
    el('comment-text').value='';show();say('Comment recorded in memory. Save to retain it.');
  };
  el('comment-load').onchange=async()=>{
    try{
      const file=el('comment-load').files[0];if(!file)return;
      const value=JSON.parse(await file.text());
      const loaded=validate(value,digest);
      if(loadedDigest&&value.report_digest&&loadedDigest!==value.report_digest)
        throw Error('Loaded comments belong to different report content.');
      loadedDigest=value.report_digest||loadedDigest;
      loadedRecord={...loadedRecord,...value};
      comments=merge(comments,loaded);show();say('Comments loaded. Save to retain the combined file.');
    }catch(error){say(error.message);}
    el('comment-load').value='';
  };
  el('comment-save').onclick=async()=>{
    try{
      let combined=comments, retainedRecord=loadedRecord;
      if(directory){
        const current=await (await directory.getFileHandle('report.html')).getFile();
        if(await hash(await current.arrayBuffer())!==digest) throw Error('Report changed. Select its folder again.');
        let retained=[];
        try{
          const existing=await (await directory.getFileHandle('report.comments.json')).getFile();
          const value=JSON.parse(await existing.text());
          retained=validate(value,digest);retainedRecord={...loadedRecord,...value};
        }catch(error){if(error.name!=='NotFoundError')throw error;}
        combined=merge(retained,comments);
      }
      const data=JSON.stringify({...retainedRecord,report:id,revision,report_digest:digest,calculation:el('report-data').textContent,comments:combined},null,2)+'\n';
      if(directory){
        const handle=await directory.getFileHandle('report.comments.json',{create:true});
        const stream=await handle.createWritable();await stream.write(data);await stream.close();
        if(await (await handle.getFile()).text()!==data)throw Error('Saved comments differ from the requested content.');
        comments=merge(combined,comments);show();
        return say('Comments saved beside report.html and read back.');
      }
      const url=URL.createObjectURL(new Blob([data],{type:'application/json'}));
      const link=document.createElement('a');link.href=url;link.download='report.comments.new.json';link.click();
      setTimeout(()=>URL.revokeObjectURL(url),1000);
      say('Download requested. Load existing comments first to combine feedback; save under a new filename beside report.html and reload to check persistence.');
    }catch(error){say(error.message);}
  };
  const changes=JSON.parse(el('change-data').textContent);
  const list=el('change-list');
  changes.revisions[0].changes.forEach(text=>{const li=document.createElement('li');li.textContent=text;list.append(li);});
})();
'''


def render(record: dict) -> str:
    """Write the engineering narrative with the complete workflow in context."""
    n = lambda key, kind="decimal": number(record, key, kind)
    theory = '<a href="#open-theory">Open theory</a>'
    example = '<a href="#public-example">Public example</a>'
    implementation = '<a href="#implementation">Implementation</a>'
    tests = '<a href="#test-evidence">Test evidence</a>'
    contents = "".join(f'<li><a href="#{key}">{title}</a></li>' for key, title in SECTIONS)
    parts = []
    parts.append(section("executive-summary", f'''
<p>The spectral fatigue module is a frequency-domain screening tool for local structural details. Analysis is performed to determine the occurrence-weighted fatigue life of the supplied stress spectra, and the key results are summarized below.</p>
<ul><li>The public example is reproduced as a screening calculation.
<ul><li>The splash-zone detail returns {n('summary.0.fatigue_life_years')} years against the required {n('summary.0.required_life_years')} years. Its life margin is {n('summary.0.margin')}, below the workflow acceptance boundary.</li>
<li>The keel joint returns {n('summary.1.fatigue_life_years')} years against the required {n('summary.1.required_life_years')} years. Its life margin is {n('summary.1.margin')}, above the workflow acceptance boundary.</li></ul></li>
<li>Design acceptance is withheld pending numerical qualification.
<ul><li>The documented hertz input convention and the implemented crossing-rate convention differ. The implemented Dirlik bandwidth substitution also differs from the open equations.</li>
<li>The failed splash-zone screen requires a revised stress response and a repeat calculation after the rate convention and estimator coefficients are resolved.</li></ul></li></ul>
<p>These values describe the current module, not the service life of an assessed structure. Basis: {example}, {implementation} and {theory}. The governing life comparison is shown below; the detailed results retain both location summaries.</p>'''))
    parts.append('''<div id="revision-history"><h3>Revision history</h3>
<table><caption>Issue record</caption><thead><tr><th scope="col">Revision</th><th scope="col">Date</th><th scope="col">What changed and why</th></tr></thead>
<tbody><tr><td>0</td><td>2026-10-21</td><td>First issue: the public calculation, evidence gaps and actions before design use are documented.</td></tr>
<tr><td>1</td><td>2026-10-25</td><td>Saved comments are preserved, and hand-calculated numerical checks are added to distinguish reproduction from reference damage.</td></tr></tbody></table></div>''')
    parts.append(section("introduction", f'''
<p>Fatigue screening is performed from local stress power spectral density, without a time history or a rainflow count. The public workflow is evaluated from the supplied spectra through the reported life and governing location.</p>
<p>The spectrum describes the distribution of stress variance over frequency. It does not describe riser geometry or calculate stress from waves. The example is openly documented in the repository and is used as a software demonstration; no measured structural response or independently published physical benchmark is supplied with it. Basis: {example}.</p>'''))
    parts.append(section("design-data", f'''
<p>The supplied stress spectra are treated as the complete input boundary. Design life, design fatigue factor, stress-life coefficients and sea-state occurrences are read directly from the public example.</p>
<h3>System and input basis</h3>
<p>A splash-zone detail and a keel joint are identified. Site, water depth, material, weld geometry, support restraint, mean stress and stress concentration treatment are not supplied. A single-sided, stationary Gaussian local stress spectrum is <span>ASSUMED - to be confirmed</span>; this is a method-applicability assumption, not a measured property. Basis: {example} and {theory}.</p>
{schematic()}
<p>Frequency is specified in hertz, stress spectral density in square megapascals per hertz, and time in years. The design life is {n('input.design_life_years')} years and the supplied design fatigue factor is {n('input.dff')}. These values are demonstration inputs. They are not project-specific acceptance criteria. Exact numeric inputs are retained in the Appendix input register and the embedded calculation data.</p>
<h3>Criteria and rationale</h3>
<p>The required life is the design life multiplied by the design fatigue factor. The life margin is calculated life divided by required life. The workflow accepts a location when this margin is at least unity. The factor provides the supplied life reserve; its engineering selection is a required input for an actual assessment. A passing screen does not establish acceptance while the numerical qualification gaps remain. Basis: {implementation}.</p>'''))
    parts.append(section("methodology", f'''
<p>Moments are obtained by trapezoidal integration on the supplied frequency grid. A damage estimator is applied to each sea state, and annual damage is weighted by its occurrence fraction before location lives are compared.</p>
<h3>Spectral basis and equations</h3>
<p>Let f be frequency in hertz and S(f) be the single-sided stress spectral density. The stress variance is m₀ and its square root is the root mean square stress. The moment order is n. The irregularity factor is α₂ and the complementary bandwidth parameter is ε.</p>
<p>mₙ = ∫ fⁿ S(f) df; σ = √m₀; α₂ = m₂ / √(m₀m₄); ε = √(1 − α₂²).</p>
<p>For moments formed on a hertz grid, the mean upcrossing rate is ν₀ = √(m₂/m₀) and the peak rate is νₚ = √(m₄/m₂), in events per second. For moments formed with angular frequency, the rates include division by 2π. The two moment conventions are related by mₙ(angular) = (2π)ⁿmₙ(hertz). These expressions follow from the frequency definition and the moment equations in {theory}.</p>
<p>The implemented library integrates powers of the supplied hertz values but divides both rates by 2π. For the public example, the difference in returned peak rate is {n('states.0.peak_difference_percent')} % relative to the rate obtained from the documented hertz convention. This is a convention discrepancy, not a validated correction to the reported fatigue lives. Basis: {implementation}; the Appendix shows both rates.</p>
<h3>Damage and life</h3>
<p>Let Δσ denote stress range, N allowable cycles, m the supplied stress-life slope, and A the stress-life intercept. The single-slope relation is N = A/(Δσ)ᵐ. The intercept A is obtained by exponentiating the supplied decimal logarithm. The narrow-band range-based expression is d = ν₀ (2√(2m₀))ᵐ Γ(1 + m/2)/A, where d is damage per second and Γ is the gamma function. This is the range form of the amplitude expression in {theory}; amplitude is half the stress range.</p>
<p>The default estimator represents the range distribution by an exponential and Rayleigh mixture. In the open formulation the first mixture weight is D₁ = 2(x − α₂²)/(1 + α₂²), with x = (m₁/m₀)√(m₂/m₄). The implementation uses ε in place of α₂ throughout the coefficient calculation, and clips several parameters. The difference is retained in this reproduction and requires an independent estimator benchmark before design use. The open equation and the code are identified in References.</p>
<p>Let pᵢ be the occurrence fraction and aᵢ the full-time annual damage in sea state i. Annual location damage is a = Σpᵢaᵢ; calculated life is L = 1/a; required life is Lᵣ = YF; and life margin is M = L/Lᵣ. Here Y denotes design life and F denotes design fatigue factor. The implementation converts seconds to years using its fixed calendar convention; that conversion is reproduced directly rather than replaced. Basis: {implementation}.</p>
<p>No bilinear stress-life knee, environment correction or thickness correction is applied by this workflow. The supplied coefficients are used as a single slope at all stress ranges.</p>'''))
    result_rows = []
    for i, row in enumerate(record["summary"]):
        prefix = f"summary.{i}"
        result_rows.append([LABELS[row["location_id"]], n(prefix+'.annual_damage', 'scientific'),
                            n(prefix+'.fatigue_life_years'), n(prefix+'.required_life_years'),
                            n(prefix+'.margin'), "The screen passes." if row["passes"] else "The screen fails.", example])
    figures = []
    for i, row in enumerate(record["summary"]):
        figures.append(f'''<figure><svg id="life-chart-{i}" viewBox="0 0 800 335" role="img" aria-label="{LABELS[row['location_id']]}: calculated life and required life"></svg>
<p id="life-hover-{i}" role="status"></p><figcaption>{LABELS[row['location_id']]}: the solid line is calculated available life. The dashed line is required life, increasing with design life assessed and the supplied fatigue factor. Hover or focus a marker for demand, limit, component and units. The final comparison uses the module-returned life and required life. Source: {example} and {implementation}.</figcaption></figure>''')
    # Place the governing comparison in the summary, with the remaining
    # location chart beside the detailed results.
    parts[0] = parts[0].replace('</section>', figures[0] + '</section>')
    # Keep the worked example entirely bound: no typed numeric results or input values.
    parts.append(section("detailed-results", f'''
<p>The documented public example is calculated through the public workflow with its numeric inputs unchanged. Only the output directory is redirected; calculation files are kept separate from the report.</p>
<p>The splash-zone detail governs. Its calculated life is {n('summary.0.fatigue_life_years')} years against {n('summary.0.required_life_years')} years required. Its screen fails; the design use restriction applies to both details.</p>
{table(['Location', 'Annual damage (per year)', 'Calculated life (years)', 'Required life (years)', 'Life margin', 'Screening result', 'Source'], result_rows, 'Location results returned by the workflow. The screen compares the life margin with unity.')}
<p>The <a href="#executive-summary">governing life chart</a> is shown in the Executive Summary.</p>
{''.join(figures[1:])}
<p>Legend: The screen passes when calculated life meets the required life. The screen fails when calculated life is below required life. Engineering acceptance is not evaluated while the documented numerical differences remain unresolved.</p>
<p>The failed screen is addressed by reducing the qualified local stress response through a revised detail, stress concentration or operating exposure, followed by recalculation against the same required-life criterion. No numerical stress reduction is specified because the current estimator has not been independently qualified. Basis: {theory} and {implementation}.</p>'''))
    parts.append(section("findings", f'''
<p>The main workflow is available and its calculation chain is exercised by repository tests. The evidence establishes software behavior within those tests, with defined limits on engineering interpretation.</p>
<p>The estimator test file contains {n('evidence.estimator_test_definitions', 'integer')} test definitions covering moment properties, positive damage, stress sensitivity, method labels and selected inter-method relationships. The workflow test compares its narrow-band annual damage with a direct call to the same library. That is a wiring regression, not an independent physical validation. Basis: {tests}.</p>
<p>The public example supplies stress spectra but no independent measured-life result or qualified physical benchmark for the default estimator. A hand calculation from the open equations is retained in the report tests to distinguish reference damage from the reproduced implementation. It does not qualify structural life. Other fatigue test files exercise separate structural implementations; those results do not qualify this workflow automatically. No experimental accuracy band is established by the evidence reviewed here.</p>
<p>The unqualified hand reference places both details below the required {n('summary.0.required_life_years')} years, including the keel joint that passes the reproduced workflow screen. Numerical correction and physical qualification remain outstanding. Basis: {tests} and {theory}.</p>'''))
    parts.append(section("limitations", f'''
<p>The outputs are restricted to reproduction of the supplied scalar spectra. Engineering acceptance is not evaluated until the frequency convention, estimator coefficients and input suitability are resolved.</p>
<p>Input guards are incomplete: frequency ordering, finite values, nonnegative spectra and occurrence fractions summing to unity are not checked. Positive occurrence fractions above unity are accepted. A zero-damage location is rejected by the workflow, so the current route does not return infinite life for a zero spectrum. Basis: {implementation}.</p>
<p>Numerical convergence of the coarse example frequency grid is not demonstrated. Gaussian stationarity, stress concentration, mean stress, structural restraint, corrosion environment, load simultaneity and the fatigue-factor selection remain unqualified. The single-slope result is not a complete standards assessment.</p>
<h3>Not covered</h3>
<p>The wave-to-stress response workflow, legacy closed-form workflow, alternate structural fatigue libraries, time-history rainflow counting, multiaxial criteria and crack-growth inspection planning are outside this report. Alternate spectral estimators are listed as available options; their individual empirical coefficients and accuracy are not evaluated here. Basis: {implementation} and {theory}.</p>'''))
    parts.append(section("recommendations", f'''
<p>Numerical qualification is required before the returned life is used for design or an operating decision. The actions below follow the documented calculation differences and the method assumptions.</p>
<ul><li>Resolve the frequency convention with a dimensionally consistent hertz and angular-frequency equivalence test. Compare crossing rates and annual damage with an independent narrow-band calculation. Basis: {theory}.</li>
<li>Compare the default mixture coefficients with the open formulation and add an independent narrow-band-limit and broad-band damage benchmark before accepting the estimator. Qualify the alternative estimators separately. Basis: {theory}.</li>
<li>For the failed splash-zone detail, reassess the local stress-transfer model and weld detail, then evaluate the revised stress spectra against the required life after numerical qualification. The keel-joint screen is retained as a numerical result only. Basis: the stress-life and damage equations in {theory}.</li>
<li>Require finite increasing frequencies, finite nonnegative spectra and a complete occurrence distribution before calculation. Add frequency-grid convergence and application-specific stress-life checks. Basis: the integration and probability assumptions in {theory}.</li>
<li>Supply the missing physical and material data in the Appendix register, select an applicable fatigue factor, and obtain independent engineering review before design release.</li></ul>'''))
    parts.append(section("references", f'''
<p>The engineering equations are linked to an open primary review article. Numeric demonstration inputs and returned values are linked to the public repository records.</p>
<h3>9.1 Engineering sources</h3>
<p id="open-theory">Dirlik and Tovo-Benasciutti Spectral Methods in Vibration Fatigue: A Review with a Historical Perspective. Metals, 2021, volume 11, article 1333. DOI: 10.3390/met11091333. The open publisher XML was read successfully. The fatigue-background section defines moment and frequency conventions, the stress-life damage basis and the narrow-band expression; the Dirlik-method section defines the mixture coefficients and damage. <a href="{REFERENCE}">Open article</a>. Exact equation locators are collected below.</p>
<p>The workflow documentation labels its demonstration coefficients as a D-curve in air. No licensed standard was opened for this report. The coefficients are treated as supplied demonstration inputs; no standard table is reproduced and no standards-acceptance claim is made.</p>
<h3>9.2 Evidence locators and reproduction</h3>
<ul><li>Open theory: article section 2, equations (1)–(5), and section 3, equations (8)–(9). The article uses stress amplitudes and angular-frequency moments; this report distinguishes the range and hertz forms explicitly.</li>
<li id="public-example">Public example: <code>examples/workflows/spectral-fatigue/README.md</code> and <code>examples/workflows/spectral-fatigue/input.yml</code>. These are the complete numeric input basis.</li>
<li id="implementation">Implementation: <code>src/digitalmodel/spectral_fatigue/workflow.py</code>, <code>router</code> and <code>_evaluate_location</code>; <code>src/digitalmodel/fatigue/spectral_fatigue.py</code>, <code>compute_spectral_moments</code>, <code>SpectralMoments</code> and <code>dirlik_damage</code>. The fixed calendar conversion is defined in each damage estimator.</li>
<li>Public call: import <code>router</code> from <code>digitalmodel.spectral_fatigue.workflow</code> and call <code>router(configuration)</code> with the loaded example dictionary. The returned dictionary replaces the original settings with a summary. Preserve a copy of the input when both are needed. The documented command-line entry is <code>python -m digitalmodel examples/workflows/spectral-fatigue/input.yml</code>.</li>
<li>Input contract: <code>spectral_fatigue</code> contains required positive <code>design_life_years</code> and <code>dff</code>, a <code>sn_curve</code> mapping with positive <code>slope</code> and <code>log_intercept</code>, and nonempty <code>locations</code>. Each location contains nonempty <code>sea_states</code>; each state supplies equal-length <code>frequency_Hz</code> and <code>stress_psd_MPa2_Hz</code> lists and an <code>occurrence_fraction</code>. Supply all occurrences explicitly. <code>method</code> selects the estimator and <code>output_dir</code> selects the output folder.</li>
<li>Output contract: the returned <code>spectral_fatigue</code> mapping contains <code>locations</code>, <code>governing_location</code>, <code>governing_margin</code>, <code>governing_fatigue_life_years</code> and <code>screening_status</code>. A per-state comma-separated file contains RMS stress, bandwidth, peak rate, full-time annual damage and occurrence-weighted annual damage. A second comma-separated file contains each location's annual damage, calculated life, required life, margin and pass flag. The returned <code>results_csv</code> and <code>summary_csv</code> fields identify those files.</li>
<li id="test-evidence">Test evidence: <code>tests/fatigue/test_spectral_fatigue.py</code>; <code>tests/workflows/test_durable_workflows.py::test_spectral_fatigue_workflow_matches_narrow_band_closed_form</code>. The adjacent <code>tests/test_spectral_fatigue.py</code> and <code>tests/test_spectral_fatigue_validation.py</code> target other implementations.</li>
<li>Rebuild: set the source import path to <code>src</code> and call <code>docs/reports/modules/spectralfatigue/build_report.py --work-dir "$TMPDIR/spectral-calculation"</code>. Rebuild tests: <code>tests/reporting/test_spectralfatigue_module_report.py</code>. The generator calls the public workflow and reads its comma-separated output.</li>
<li>Presentation exemplar: <code>docs/reports/pipeline-ffs-assessment.html</code>. Its stylesheet is inlined unchanged and the required report body class is applied.</li></ul>'''))
    parts.append(section("acronyms", '''
<p>The symbols and units used in the assessment are defined here. Stress is expressed in megapascals and frequency in hertz throughout the input and result tables.</p>
<p>Power spectral density (PSD) is stress variance per unit frequency. Root mean square (RMS) stress is the square root of stress variance. The design fatigue factor (DFF), denoted F, multiplies the design life Y to obtain required life Lᵣ. The life margin M is calculated life L divided by Lᵣ.</p>
<p>f is ordinary frequency; ω is angular frequency; S is a single-sided stress spectrum; mₙ is a spectral moment of order n; σ is RMS stress; α₂ is the irregularity factor; ε is its complementary bandwidth parameter; ν₀ and νₚ are upcrossing and peak rates. Δσ is stress range; N is allowable cycles; A and m are stress-life coefficients; Γ is the gamma function; x is the moment combination used in the mixture; D₁ is its first weight. d is damage per second; a is annual damage; aᵢ is full-time annual sea-state damage; pᵢ is sea-state occurrence fraction; i indexes the sea states. The symbol π is the circle constant. MPa denotes megapascal; Hz denotes hertz.</p>'''))
    input_rows = []
    for li, location in enumerate(record["input"]["locations"]):
        for si, sea_state in enumerate(location["sea_states"]):
            base = f"input.locations.{li}.sea_states.{si}"
            for fi in range(len(sea_state["frequency_Hz"])):
                input_rows.append([LABELS[location["id"]], "Mild" if li == 0 and si == 0 else "Severe" if li == 0 else "Operating",
                                   n(base+'.occurrence_fraction', 'hundredths'), n(base+f'.frequency_Hz.{fi}', 'hundredths'),
                                   n(base+f'.stress_psd_MPa2_Hz.{fi}'), example])
    state_rows, audit_rows = [], []
    for i, state in enumerate(record["states"]):
        b = f"states.{i}"
        state_rows.append([state["label"], "Mild" if i == 0 else "Severe" if i == 1 else "Operating",
                           n(b+'.rms'), n(b+'.epsilon'), n(b+'.result.damage_per_year', 'scientific'), example])
        audit_rows.append([state["label"], "Mild" if i == 0 else "Severe" if i == 1 else "Operating",
                           n(b+'.peak_rate', 'scientific'), n(b+'.hertz_peak_rate', 'scientific'),
                           n(b+'.peak_difference_percent'), implementation + "; " + theory])
    parts.append(f'''<section id="appendix"><h2>Appendices</h2>
<p>The detailed data and evidence status are retained here. The embedded calculation record contains full-precision inputs, moments, estimator results, annual-damage rows and location summaries.</p>
<h3>Input and calculation register</h3>
<p>The single-slope stress-life coefficients are retained exactly as supplied in the public example: slope {n('input.sn_curve.slope', 'input')}, decimal logarithm of intercept {n('input.sn_curve.log_intercept', 'input')}. These values are demonstration inputs, not a qualified material selection. Source: {example}.</p>
{table(['Location', 'State', 'Occurrence fraction', 'Frequency (Hz)', 'Stress PSD (MPa²/Hz)', 'Source'], input_rows, 'Exact input samples. One frequency convention and stress unit system are used.')}
{table(['Location', 'State', 'RMS stress (MPa)', 'Bandwidth parameter', 'Full-time annual damage (per year)', 'Source'], state_rows, 'Per-sea-state library results before occurrence weighting.')}
{table(['Location', 'State', 'Returned peak rate (Hz)', 'Hertz-basis peak rate (Hz)', 'Difference from hertz basis (%)', 'Source'], audit_rows, 'Frequency-convention diagnostic. The difference is computed before rounding; it is not a revised fatigue prediction.')}
<h3>Data register</h3>
{table(['Data', 'Status', 'Effect and required action', 'Source'], [
 ['Stress spectra and occurrence', 'ASSUMED - to be confirmed', 'Public demonstration values; qualify local stress range, stationarity, spectrum sidedness and annual exposure.', example],
 ['Material, weld and environment', 'NOT_EVALUATED', 'Supply an applicable stress-life basis, environment, thickness and stress concentration treatment before design use.', example],
 ['Physical model and restraints', 'NOT_EVALUATED', 'Supply geometry, supports and the derivation of each local stress spectrum.', example],
 ['Fatigue factor and design life', 'ASSUMED - to be confirmed', 'Demonstration inputs are supplied; approve application-specific criteria before assessment.', example],
 ['Independent physical benchmark and convergence', 'NOT_EVALUATED', 'The tests retain a hand-calculated numerical reference; provide a qualified physical benchmark and frequency-grid convergence study after resolving the implementation differences.', tests + '; ' + theory],
 ], 'Data held and data needed. No missing physical quantity is assigned a guessed numeric value.')}
<h3>QA pre-flight</h3>
{table(['Check', 'Evidence and outcome', 'Source'], [
 ['Public entry point', 'The documented example reaches the occurrence-weighted life calculation; the generator calls the same public router directly.', example + '; ' + implementation],
 ['Numeric reproduction', 'Every displayed example value is bound to the embedded record; direct-call regressions are retained. Hand-calculated literals independently check moments, occurrence-weighted annual damage and life, with reference damage kept separate from reproduction.', tests],
 ['Analytical reference', 'The moment-rate convention and mixture coefficients are compared with the open equations. Engineering qualification remains pending.', theory],
 ['Existing regression evidence', 'Estimator tests establish properties and selected relationships; the workflow test checks agreement with its own library, not an external result.', tests],
 ['Presentation and comments', 'The report is self-contained. Comments are saved beside the selected report where folder access is supported; the fallback download requires selection of the same folder. Comprehensive visual review is not recorded.', 'This report'],
 ], 'Pre-flight evidence and its limits. Reproduction does not establish physical accuracy.')}
</section>''')
    figures_json = encode(chart_data(record))
    return f'''<!DOCTYPE html>
<html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">
<title>Spectral fatigue module analysis</title>
{house_style()}
</head><body class="hs-doc">
<header><h1>Spectral fatigue module analysis</h1><p>Draft for maintainer review</p><p>Public example reproduction and limits of use</p></header>
<div class="layout"><nav aria-label="Contents"><ol>{contents}<li><a href="#appendix">Appendices</a></li></ol>
<details><summary>Changes since the previous issue</summary><ul id="change-list"></ul></details></nav>
<main>{''.join(parts)}
<aside id="comments" aria-label="Report comments"><h2>Report comments</h2>
<p>Choose the folder containing report.html to save comments beside it. Existing comments for matching report content are merged; identical section and text pairs are combined. Keep files from other report revisions or rebuilds with changed content separately. Where folder access is unavailable, load earlier comments before downloading and save under a new filename in the report folder. Save from one window at a time. Reload saved comments to confirm persistence. Comments remain on this device.</p>
<button id="comment-folder" type="button">Choose report folder</button>
<label>Section <select id="comment-section"></select></label>
<label>Comment <textarea id="comment-text" maxlength="10000"></textarea></label>
<button id="comment-add" type="button">Add comment</button>
<button id="comment-save" type="button">Save comments</button>
<label>Load saved comments <input id="comment-load" type="file" accept=".json"></label>
<p id="comment-status" role="status">No report folder selected.</p><pre id="comment-preview"></pre></aside></main></div>
<script id="report-data" type="application/json">{encode(record)}</script>
<script id="change-data" type="application/json">{encode(CHANGES)}</script>
<script id="chart-data" type="application/json">{figures_json}</script>
<script>{CHART_SCRIPT}</script><script>{COMMENT_SCRIPT}</script>
</body></html>
'''


def build(output_dir: Path, work_dir: Path) -> None:
    """Write only the report and its engineering change history."""
    record = calculate(work_dir)
    output_dir.mkdir(parents=True, exist_ok=True)
    (output_dir / "report.html").write_text(render(record), encoding="utf-8", newline="\n")
    (output_dir / "changes.json").write_text(json.dumps(CHANGES, indent=2) + "\n", encoding="utf-8", newline="\n")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", type=Path, default=HERE)
    parser.add_argument("--work-dir", type=Path, required=True)
    args = parser.parse_args()
    build(args.output_dir, args.work_dir)
