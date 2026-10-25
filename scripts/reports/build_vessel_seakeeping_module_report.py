"""Build a self-contained report by executing the documented public example.

Run with source imports available and --scratch pointing outside the checkout.
The public input is illustrative; no physical benchmark is inferred from it.
"""
import argparse
import copy
import csv
import hashlib
import html
import json
import re
from pathlib import Path

import yaml

ROOT = Path(__file__).resolve().parents[2]
EXAMPLE = "examples/workflows/vessel-seakeeping/input.yml"
DESTINATION = ROOT / "docs/reports/modules/vesselseakeeping"
CHANGES = [{"revision": "A", "date": "2026-10-21", "change":
            "The spectral operability method, illustrative calculation and limits of use are presented for initial assessment."},
           {"revision": "B", "date": "2026-10-24", "change":
            "The shared workflow test source digest is refreshed after unrelated test changes to restore exact report rebuilds; calculation inputs and results are unchanged."}]
SOURCES = [EXAMPLE, "examples/workflows/vessel-seakeeping/README.md",
           "src/digitalmodel/vessel_seakeeping/workflow.py",
           "src/digitalmodel/hydrodynamics/seakeeping.py",
           "src/digitalmodel/hydrodynamics/wave_spectra.py",
           "tests/hydrodynamics/test_seakeeping.py",
           "tests/workflows/test_durable_workflows.py"]


def calculate(scratch):
    """Call the actual workflow, retaining output files outside the checkout."""
    from digitalmodel.vessel_seakeeping.workflow import router

    scratch = Path(scratch).resolve()
    if scratch == ROOT or ROOT in scratch.parents:
        raise ValueError("Calculation scratch must be outside the checkout")
    cfg = yaml.safe_load((ROOT / EXAMPLE).read_text())
    inputs = copy.deepcopy(cfg["vessel_seakeeping"])
    inputs.pop("vessel")
    inputs.pop("output_dir")
    cfg["vessel_seakeeping"]["output_dir"] = str(scratch)
    result = router(cfg)["vessel_seakeeping"]
    with Path(result["results_csv"]).open() as stream:
        raw = list(csv.DictReader(stream))
    rows = []
    for row in raw:
        for key in ("hs", "tp", "probability", "significant_amplitude", "criterion"):
            row[key] = float(row[key])
        row["operable"] = row["operable"] == "True"
        rows.append(row)
    return {"inputs": inputs, "rows": rows, "summary": result["dofs"],
            "status": result["screening_status"]}


def result_row(row):
    """Format every worked result directly from its returned value."""
    return (f'<tr><td>{html.escape(row["dof"])}</td><td>{row["hs"]:.1f}</td>'
            f'<td>{row["tp"]:.1f}</td><td>{row["probability"]:.1%}</td>'
            f'<td>{row["significant_amplitude"]:.3f}</td>'
            f'<td>{row["criterion"]:.3f}</td><td>{html.escape(row["unit"])}</td>'
            f'<td>{"The criterion is met." if row["operable"] else "The criterion is exceeded."}</td>'
            '<td>[A, C]</td></tr>')


def summary_row(row):
    """Format operability and its required value from the workflow summary."""
    return (f'<tr><td>{html.escape(row["dof"])}</td>'
            f'<td>{row["operability_pct"]:.1f}</td>'
            f'<td>{row["required_operability_pct"]:.1f}</td>'
            f'<td>{"The requirement is met." if row["passes"] else "The requirement is not met."}</td>'
            '<td>[A, C]</td></tr>')


def render(scratch):
    """Write the report narrative once, around computed tables and figures."""
    data = calculate(scratch)
    summaries = "".join(summary_row(row) for row in data["summary"])
    results = "".join(result_row(row) for row in data["rows"])
    governing = min(data["summary"], key=lambda row: row["operability_pct"])
    g = html.escape(governing["dof"])
    pct = f'{governing["operability_pct"]:.1f}'
    required = f'{governing["required_operability_pct"]:.1f}'
    input_json = html.escape(json.dumps(data["inputs"], indent=2))
    provenance = "".join(f'<li>{html.escape(path)}; SHA-256: '
                         f'{hashlib.sha256((ROOT / path).read_bytes()).hexdigest()}</li>'
                         for path in SOURCES)
    # Reuse the existing report layout and canonical palette, inlined for delivery.
    exemplar = (ROOT / "docs/api/cfd/turbulent-flat-plate-verification.html").read_text()
    layout = re.search(r"<style>(.*?)</style>", exemplar, re.S)[1]
    brand = re.sub(r"/\*.*?\*/", "", (ROOT / "docs/api/_assets/brand.css").read_text(), flags=re.S)
    payload = json.dumps(data, sort_keys=True, allow_nan=False).replace("<", "\\u003c")
    changes = json.dumps(CHANGES).replace("<", "\\u003c")
    history = "".join(
        f'<tr><td>{html.escape(item["revision"])}</td><td>{html.escape(item["date"])}</td>'
        f'<td>{html.escape(item["change"])}</td></tr>' for item in CHANGES[1:]
    )
    return f'''<!DOCTYPE html>
<html lang="en" data-theme="light"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Vessel seakeeping module analysis</title><style>{brand}\n{layout}</style></head>
<body class="hs-doc"><main class="wrap">
<header><h1>Vessel seakeeping module analysis</h1><p>Draft for client review</p>
<p>Frequency-domain motion screening from supplied response amplitudes and sea-state occurrence.</p></header>
<h2 id="s1">1. Executive Summary</h2>
<p>The vessel seakeeping module is a linear spectral operability calculator. Analysis is performed to determine motion criterion exceedance across an illustrative sea-state distribution; key results are summarized below.</p>
<ul><li>Screening against the supplied criteria is available.<ul><li>The governing component is {g}, with operability of {pct} % against a required {required} % [A, C]. These illustrative criteria are not operating limits for a physical hull.</li></ul></li>
<li>Engineering release remains conditional.<ul><li>Physical response accuracy is NOT_EVALUATED because measured motions, qualified response amplitudes and approved operating criteria are absent [B].</li></ul></li></ul>
<h3>Revision history</h3><table><thead><tr><th>Revision</th><th>Date</th><th>Change and cause</th></tr></thead>
<tbody><tr><td>A</td><td>2026-10-21</td><td>The method and illustrative calculation are presented to establish the scope and limits of motion screening.</td></tr>{history}</tbody></table>
<h2 id="s2">2. Introduction</h2>
<p>The purpose, scope and status of the analysis are presented in this section. The documented public example is evaluated through the module entry point, without generating hydrodynamic response amplitudes.</p>
<p>The main workflow converts supplied motion response amplitude operators and wave conditions into significant motion amplitudes, component operability and a governing disposition [B, C]. A response amplitude operator is the motion amplitude per unit incident wave amplitude. The example describes an illustrative floating hull; its response curves are not measured hull data.</p>
<h2 id="s3">3. Design Data and Basis</h2>
<p>The input basis is taken from the documented public example [A, B]. All response amplitudes, wave conditions, occurrence weights and criteria are ASSUMED - to be confirmed for physical use.</p>
<p>Heave is vertical translation; roll is rotation about the longitudinal axis; pitch is rotation about the transverse axis. Translational response amplitudes use metres per metre and rotational response amplitudes use degrees per metre. Response amplitude units must match the criterion units [C, D].</p>
<figure><svg viewBox="0 0 760 210" role="img" aria-label="Conceptual floating hull with incident waves, heave, roll and pitch; not a geometric model">
<path d="M25 118 Q45 96 65 118 T105 118 T145 118 T185 118 T225 118 T265 118 T305 118 T345 118 T385 118 T425 118 T465 118 T505 118 T545 118 T585 118 T625 118 T665 118 T705 118" fill="none" stroke="currentColor"/>
<path d="M255 100 L535 100 L510 157 L285 157 Z M310 100 L310 65 L465 65 L465 100" fill="var(--panel-2)" stroke="currentColor"/>
<path d="M395 37 L395 185 M385 50 L395 37 L405 50 M385 172 L395 185 L405 172" fill="none" stroke="currentColor"/>
<text x="415" y="40">Heave: vertical motion</text><text x="25" y="70">Incident irregular waves →</text>
<text x="255" y="200">Pitch: transverse rotation; roll: longitudinal rotation</text>
</svg><figcaption>Conceptual motion system; dimensions, heading and boundary conditions are not supplied [B].</figcaption></figure>
<p>A component is operable when its significant single amplitude does not exceed its supplied criterion. Occurrence-weighted operability must meet the supplied requirement for every component [C]. The limits are demonstration inputs, not limits established by a standard.</p>
<h2 id="s4">4. Analysis Methodology</h2>
<p>The response spectrum is formed by linear transfer of the wave spectrum [D]. Spectral integration and occurrence weighting are then applied independently to each component [C, D].</p>
<div class="eq">Sᵣ(ω) = |R(ω)|² Sη(ω); mₙ = ∫ ωⁿ Sᵣ(ω) dω; aₛ = 2√m₀ [D].</div>
<p>Here ω is angular frequency in radians per second, R is the response amplitude operator, Sη is wave elevation spectral density, Sᵣ is motion spectral density, n is the spectral moment order, m₀ is motion variance and aₛ is significant single amplitude. The coefficient in the amplitude expression is the implementation convention [D], based on narrow-band Gaussian response; it is not a peak-to-peak motion.</p>
<div class="eq">O = 100 × Σ pᵢ χ(aₛ,ᵢ ≤ c) / Σ pᵢ [D].</div>
<p>O is operability in percent, pᵢ is occurrence weight for sea state i, c is the component criterion and χ is the indicator of criterion satisfaction. The percentage conversion is definitional. Sea-state weights are normalized by their sum, so the result is a fraction of the supplied distribution rather than an annual weather forecast.</p>
<p>The selected wave spectrum is generated on a uniform angular-frequency grid with the supplied endpoints and point count [D, E]. Its integrated energy is normalized to the supplied significant wave height through mη = Hₛ²/16, where Hₛ is significant wave height and mη is elevation variance [E]. Trapezoidal integration is used. The normalization redistributes energy within the supplied band and does not establish spectral-tail convergence.</p>
<p>The published implementation is the equation source for this software analysis [C–E]. External standard editions and clauses have not been verified; conformity with a standard is NOT_EVALUATED. Empirical spectrum coefficients are not independently prescribed by this report.</p>
<h2 id="s5">5. Detailed Results</h2>
<p>The documented illustrative example is calculated by calling the module router [A–C]. The returned component operability is compared with the returned required percentage below.</p>
<table><thead><tr><th>Component</th><th>Operability (%)</th><th>Required (%)</th><th>Disposition</th><th>Source</th></tr></thead><tbody>{summaries}</tbody></table>
<p>The chart compares significant single amplitude with the supplied criterion. Each point represents one sea state; its peak period and occurrence are retained in the hover description.</p>
<label for="component">Motion component</label><select id="component"></select>
<figure><svg id="response-chart" viewBox="0 0 760 330" role="img" aria-label="Significant motion amplitude against significant wave height with criterion"></svg>
<figcaption>Significant motion amplitude against wave height; the dashed line is the illustrative criterion [A, C]. Select a component; hover or focus each point for values.</figcaption></figure>
<p>The governing component is {g}, with operability of {pct} % against a required {required} % [A, C]. These illustrative criteria are not operating limits for a physical hull.</p>
<h2 id="s6">6. Key Findings</h2>
<p>The calculation establishes software behavior for the supplied illustrative inputs. Physical response accuracy is NOT_EVALUATED because measured motions, qualified response amplitudes and approved operating criteria are absent [B].</p>
<ul><li>Component screening is available.<ul><li>The wrapper applies a separate response amplitude curve and criterion to each component and reports the least operable component [C].</li></ul></li>
<li>Numerical evidence is limited to the implemented model.<ul><li>Existing tests check unity and zero response, squared-amplitude scaling, flat-spectrum integration, known amplitude and Rayleigh values, probability rejection and operability behavior [F]. The workflow integration test compares its significant amplitude with a recomputation through the same library [G]; this is a wiring check, not independent physical validation.</li></ul></li></ul>
<h2 id="s7">7. Limitations and Not Covered</h2>
<p>The applicability limits are governed by the supplied response data and linear spectral assumptions [B–E]. Geometry, displacement, heading, speed and measured motion are absent; resistance, effective power, mesh quality and model-to-full-scale scaling are NOT_EVALUATED because this workflow performs no computational fluid dynamics or hull-scale transformation.</p>
<p>Nonuniform input frequencies are accepted by the wrapper but replaced by a uniform grid inside the spectral engine. Nonuniform response data therefore require resampling before use; the present example uses a uniform grid [A, C–E].</p>
<p>Finite-band normalization and coarse sampling leave resonance and spectral-tail uncertainty unquantified [E]. The result is not a convergence study. Directional spreading, encounter-frequency transformation, coupled response phase, nonlinear roll, slamming, green water, acceleration limits and motion time histories are not calculated by this workflow [C, D].</p>
<p>Not covered: other naval-architecture utilities, response amplitude generation and the lower-level Rayleigh exceedance function. The wrapper screens components individually; it does not calculate joint exceedance probability or a simultaneous operability percentage across components.</p>
<h2 id="s8">8. Recommendations</h2>
<p>The next engineering assessment is defined by the failed illustrative screening and the missing physical basis. The following actions follow the input requirements and calculation limitations [B–E].</p>
<p>For any component below its required operability, retain the failing sea states and establish an operating exclusion until qualified response data and approved criteria are supplied. For the illustrative roll resonance, assess heading, damping or operational changes using new response amplitudes, then repeat the criterion comparison [B, C]; a mitigation benefit is not established by the present calculation.</p>
<p>Before physical use, supply response amplitudes for the intended loading, speed and heading; confirm rotational units; reject non-finite inputs; and resample onto a uniform grid. Refine the frequency grid and extend its band until the selected motion statistic stabilizes against a maintainer-approved tolerance. Obtain a measured or independently published response case and compare motion amplitudes before adopting operating limits. No numerical release tolerance is inferred here.</p>
<h2 id="s9">9. References and Appendices</h2>
<p>The source register and calculation data are retained for inspection. Repository sources define the software behavior; they do not establish external standard compliance.</p>
<h3>9.1. Data register and detailed calculation</h3>
<p>The public illustrative example is the authoritative input for this calculation [A, B]. Readiness is partial for software demonstration and unavailable for physical acceptance. All numeric inputs below are ASSUMED - to be confirmed for physical use. Missing geometry, environmental survey, response qualification and operating-limit approval are required inputs, not numerical zeroes.</p>
<details><summary>Input data and units</summary><pre>{input_json}</pre></details>
<table><thead><tr><th>Component</th><th>Wave height (m)</th><th>Peak period (s)</th><th>Occurrence (%)</th><th>Amplitude</th><th>Criterion</th><th>Unit</th><th>Disposition</th><th>Source</th></tr></thead><tbody>{results}</tbody></table>
<p>Amplitudes and criteria use the unit in each row. Rounded displays are generated from the full returned values; comparisons use unrounded values. The criterion is met when the amplitude is at or below the criterion. The criterion is exceeded when the amplitude is above it.</p>
<h3>9.2. Source locators and quality evidence</h3>
<p>The reference edition is the repository content identified by the following file digests. No licensed publication is reproduced.</p>
<ol type="A"><li>Public illustrative input; all input arrays, coefficients, weights and criteria.</li>
<li>Public example documentation; workflow purpose and illustrative-data limitation.</li>
<li>Workflow router; component iteration, input validation, output structure and overall disposition.</li>
<li>Spectral engine; response transfer, integration, amplitude convention and weighted operability.</li>
<li>Wave spectrum implementation; selected spectrum shape, uniform grid and discrete energy normalization.</li>
<li>Existing spectral unit tests; analytical and behavioral verification cases.</li>
<li>Existing durable workflow test, significant-amplitude comparison; common-library wiring evidence.</li></ol>
<ol>{provenance}</ol>
<p>Layout exemplar: docs/api/cfd/turbulent-flat-plate-verification.html. Canonical palette: docs/api/_assets/brand.css. The existing layout and palette are inlined; the document body carries the house-style class.</p>
<p>The callable entry point is <code>digitalmodel.vessel_seakeeping.workflow.router(cfg)</code>. It mutates and returns the configuration and writes detailed and summary comma-separated value files to the configured output directory. The input keys appear in the data register; the returned component summaries and sea-state rows are reproduced above [C]. Rebuild with source imports enabled using <code>python scripts/reports/build_vessel_seakeeping_module_report.py --scratch ../report-calculation</code>; NumPy, SciPy and PyYAML are required. The scratch directory must be outside the checkout.</p>
<p>Quality pre-flight: the rebuild test compares the complete delivered page with a fresh calculation, checks every embedded input and returned result against an independent router call, and verifies the visible result rows. The mutation test confirms that a changed computed amplitude changes the display. These checks establish reproducibility, not physical accuracy. Execution counts and outcomes are recorded with the delivery record.</p>
<details id="changes-panel"><summary>Engineering changes</summary><p>{html.escape(CHANGES[-1]["change"])}</p></details>
<h2 id="s10">10. Acronyms and Review Notes</h2>
<p>The terminology is defined for interpretation of the screening result. A degree of freedom is an independent translation or rotation; a response amplitude operator is a motion-to-wave amplitude ratio; computational fluid dynamics is a numerical flow-field calculation and is not performed here.</p>
<label for="notes">Review notes</label><textarea id="notes" rows="6" style="width:100%" placeholder="Enter engineering comments"></textarea>
<button id="save-notes" type="button">Save comments beside report</button><p id="save-status" aria-live="polite">Comments are not saved until a file is written. Select the report folder when saving; browsers without a file picker download a comments file for manual placement beside the report.</p>
</main><script id="calculation" type="application/json">{payload}</script>
<script id="changes" type="application/json">{changes}</script><script>{JAVASCRIPT}</script></body></html>
'''


JAVASCRIPT = r'''
const calculation = JSON.parse(document.getElementById('calculation').textContent);
const select = document.getElementById('component');
calculation.summary.forEach(row => {
  const option = document.createElement('option');
  option.textContent = row.dof; option.value = row.dof; select.append(option);
});
function drawChart() {
  const svg = document.getElementById('response-chart'); svg.replaceChildren();
  const rows = calculation.rows.filter(row => row.dof === select.value);
  const limit = rows[0].criterion, unit = rows[0].unit;
  const xmax = Math.max(...rows.map(row => row.hs)) * 1.1;
  const ymax = Math.max(limit, ...rows.map(row => row.significant_amplitude)) * 1.2;
  const x = value => 80 + value / xmax * 630, y = value => 270 - value / ymax * 220;
  function element(tag, attrs, text) {
    const node = document.createElementNS('http://www.w3.org/2000/svg', tag);
    Object.entries(attrs).forEach(([key, value]) => node.setAttribute(key, value));
    if (text !== undefined) node.textContent = text;
    svg.append(node); return node;
  }
  element('path', {d:'M80 45 V270 H720', fill:'none', stroke:'currentColor'});
  element('text', {x:80,y:25}, `Significant amplitude (${unit})`);
  element('text', {x:300,y:320}, 'Significant wave height (m)');
  element('path', {d:`M80 ${y(limit)} H720`, stroke:'currentColor','stroke-dasharray':'6 4'});
  element('text', {x:85,y:y(limit)-8}, `Criterion ${limit.toFixed(3)} ${unit}`);
  rows.forEach(row => {
    element('text',{x:x(row.hs)-8,y:290},row.hs.toFixed(1));
    const point = element('circle',{cx:x(row.hs),cy:y(row.significant_amplitude),r:7,
      fill:'var(--brand)',tabindex:0});
    const title = document.createElementNS('http://www.w3.org/2000/svg','title');
    title.textContent = `Case: wave height ${row.hs.toFixed(1)} m; period ${row.tp.toFixed(1)} s; ` +
      `occurrence ${(row.probability*100).toFixed(1)} %; component ${row.dof}; ` +
      `demand ${row.significant_amplitude.toFixed(3)} ${unit}; limit ${limit.toFixed(3)} ${unit}`;
    point.append(title); point.setAttribute('aria-label',title.textContent);
    element('text',{x:x(row.hs)+10,y:y(row.significant_amplitude)},row.significant_amplitude.toFixed(3));
  });
}
select.addEventListener('change',drawChart); drawChart();
document.getElementById('save-notes').addEventListener('click',async () => {
  const text = JSON.stringify({document:'Vessel seakeeping module analysis', revision:'B',
    comments:document.getElementById('notes').value},null,2);
  const status = document.getElementById('save-status');
  try {
    if (window.showSaveFilePicker) {
      const handle = await window.showSaveFilePicker({suggestedName:'comments.json',
        types:[{description:'JSON comments',accept:{'application/json':['.json']}}]});
      const writable = await handle.createWritable(); await writable.write(text); await writable.close();
      status.textContent = 'Comments were saved to the selected file.';
    } else {
      const url = URL.createObjectURL(new Blob([text],{type:'application/json'}));
      const link = document.createElement('a'); link.href=url; link.download='comments.json'; link.click();
      setTimeout(() => URL.revokeObjectURL(url),1000);
      status.textContent = 'A comments download was requested. Place the file beside the report and confirm it is present.';
    }
  } catch (error) { status.textContent = 'Comments were not saved. Retry and select a writable file.'; }
});
'''


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--scratch", type=Path, required=True)
    parser.add_argument("--output-dir", type=Path, default=DESTINATION)
    args = parser.parse_args()
    page = render(args.scratch)
    args.output_dir.mkdir(parents=True, exist_ok=True)
    (args.output_dir / "index.html").write_text(page)
    (args.output_dir / "changes.json").write_text(json.dumps(CHANGES, indent=2) + "\n")


if __name__ == "__main__":
    main()
