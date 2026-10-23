"""Rebuild the self-contained motion forecast report from the public example.

Usage: python scripts/build_motionforecast_report.py [output_directory]
Only the report and its engineering change record are written.
"""
import ast
import hashlib
import json
import re
import sys
from pathlib import Path

import numpy as np
from digitalmodel.motion_forecast import (
    AnalyticRAO, WaveComponent, WaveForecast, reconstruct_motion,
)

ROOT = Path(__file__).resolve().parents[1]
EXAMPLE = 'tests/motion_forecast/test_reconstruct.py'
STYLE = 'docs/reports/pipeline-ffs-assessment.html'
DEST = ROOT / 'docs/reports/modules/motionforecast'


def example():
    """Read literal parameters from the published regular-wave test, then call API."""
    tree = ast.parse((ROOT / EXAMPLE).read_text(encoding='utf-8'))
    fn = next(n for n in tree.body if isinstance(n, ast.FunctionDef)
              and n.name == 'test_regular_wave_amplitude_and_phase')
    values = {}
    for node in fn.body:
        if isinstance(node, ast.Assign) and isinstance(node.targets[0], ast.Tuple):
            values.update(zip((n.id for n in node.targets[0].elts),
                              ast.literal_eval(node.value)))
    calls = [n for n in ast.walk(fn) if isinstance(n, ast.Call)]
    def keyword(call_name, name):
        call = next(c for c in calls if isinstance(c.func, ast.Name) and c.func.id == call_name)
        return ast.literal_eval(next(k.value for k in call.keywords if k.arg == name))
    p = dict(omega=values['w0'], amplitude=values['a0'], phase=values['phi0'],
             gain=values['mag'], response_phase=values['ang'],
             horizon=keyword('WaveForecast', 'horizon'),
             dt=keyword('reconstruct_motion', 'dt'))
    wave = WaveForecast([WaveComponent(p['omega'], p['amplitude'], p['phase'])],
                        horizon=p['horizon'])
    rao = AnalyticRAO({'heave': lambda w, b: p['gain'] * np.exp(1j * p['response_phase'])})
    motion = reconstruct_motion(wave, rao, dt=p['dt'])
    # Bound serialized precision so last-bit library differences do not cause drift.
    return dict(inputs=p, time=[round(value, 12) for value in motion.t.tolist()],
                heave=[round(value, 12) for value in motion.dof['heave'].tolist()],
                peak=round(motion.max_abs('heave'), 12),
                significant=round(motion.significant('heave'), 12))


def section(number, title, body):
    return f'<section id="section-{number}"><h2>{number}. {title}</h2>{body}</section>'


def build(destination=DEST):
    data = example()
    p = data['inputs']
    style = re.search(r'<style>.*?</style>', (ROOT / STYLE).read_text(encoding='utf-8'), re.S).group()
    changes = {'revision': '01', 'date': '2026-10-22', 'changes': [
        'Phased motion reconstruction is described with a computed regular-wave example.',
        'Synthetic calculation evidence is separated from operational qualification.',
        'Serialized precision, UTF-8 encoding and LF line endings support exact regeneration.',
        'Independent statistics and a significant-amplitude reference clarify the numerical basis.',
    ]}
    inputs = [('Angular frequency', 'omega', 'rad/s'), ('Wave amplitude', 'amplitude', 'm'),
              ('Incident phase', 'phase', 'rad'), ('Heave transfer amplitude', 'gain', 'm/m'),
              ('Heave transfer phase', 'response_phase', 'rad'),
              ('Supplied forecast interval', 'horizon', 's'), ('Requested time step', 'dt', 's')]
    rows = ''.join(f'<tr><td>{label}</td><td>{p[key]:.3f}</td><td>{unit}</td>'
                   '<td>Regular-wave example; see section 9.2.</td></tr>'
                   for label, key, unit in inputs)
    peak = f'<span data-result="peak">{data["peak"]:.3f}</span>'
    significant = f'<span data-result="significant">{data["significant"]:.3f}</span>'
    body = section(1, 'Executive Summary', f'''
<p>Motion forecasting is the reconstruction of future rigid-body displacement from phased waves and a complex response amplitude operator. Analysis is performed to determine whether the implemented calculation reproduces its documented regular-wave example; key results are summarized below.</p>
<ul><li>The synthetic heave example has a sampled peak absolute displacement of {peak} m.
<ul><li>The result is a calculation demonstration; an installation sea-state limit is not established.</li></ul></li>
<li>The supplied forecast interval is enforced by the reconstruction interface.
<ul><li>Use the calculation for software assessment within that interval; obtain independently qualified wave phases, response data and operating criteria before an installation decision.</li></ul></li></ul>''')
    body += '''<div id="revision-history"><h3>Revision history</h3><table>
<thead><tr><th>Revision</th><th>Date</th><th>Change and reason</th></tr></thead>
<tbody><tr><td>00</td><td>2026-10-22</td><td>Initial calculation description and example; operational evidence gaps are identified.</td></tr>
<tr><td>01</td><td>2026-10-22</td><td>Numerical serialization and text output are made reproducible across platforms; independent statistics and a significant-amplitude reference clarify the basis.</td></tr></tbody>
<caption>Revision history.</caption></table></div>'''
    body += section(2, 'Introduction', '''
<p>The main workflow is assessed from incident-wave components to heave displacement. The implementation also returns surge, sway, roll, pitch and yaw when response data are supplied. A response amplitude operator is the complex ratio of response to incident wave amplitude; its magnitude scales displacement and its argument adds phase.</p>
<p>The public example is the repository's documented regular-wave analytical check, not a measured installation or an externally published experiment. No actual structure, crane or rigging geometry is supplied. A dimensioned installation schematic is therefore not established.</p>''')
    body += section(3, 'Summary and use controls', '''
<p>The motion calculation is bounded by supplied input conventions and a supplied forecast interval. Operating acceptance is not inferred from a successful numerical calculation.</p>
<p>Specify the phase-reference position, common horizontal frame, propagation heading, time origin and response phase convention before use. Confirm the response grid covers every incident component. Supply equipment-specific limits and measurement-based forecast skill before setting a sea-state envelope. An orientation tolerance and a trigger for forecast support are NOT_EVALUATED because station-keeping data and installation criteria are absent.</p>''')
    body += section(4, 'Design data and criteria', f'''
<p>The regular-wave inputs are read directly from the documented test. They are synthetic demonstration inputs, not environmental design values or standards-derived limits.</p>
<table><thead><tr><th>Quantity</th><th>Value</th><th>Unit</th><th>Source</th></tr></thead><tbody>{rows}</tbody><caption>Table 4.1. Computed-example input register.</caption></table>
<p>The example uses the default coincident wave-reference and response origin, default propagation heading and deep-water dispersion. Only the heave response is supplied; omitted responses are zero by the analytical adapter's definition, not evidence of negligible physical motion. Equipment capacities, significant wave height, peak period, forecast accuracy requirements and acceptance limits are NOT_EVALUATED. No numerical installation criterion is assumed.</p>''')
    body += section(5, 'Theory and methodology', '''
<p>Linear harmonic superposition is applied to the phased components. The implemented definitions are cited in section 9.2; no claim of conformity with a licensed standard is made.</p>
<p>For component i, θᵢ(t) = ωᵢt + φᵢ − kᵢ dᵢ·(x − xᵣ) + arg Hᵢ, and q(t) = Σ aᵢ |Hᵢ| cos θᵢ(t). Here t is absolute time in seconds, ω is angular frequency in radians per second, φ is incident phase in radians, a is wave amplitude in metres, d is the unit propagation vector, x is response position and xᵣ is wave-reference position in metres. H is the complex displacement response amplitude operator; q is the selected response. Translations are in metres and rotations in degrees.</p>
<p>Deep-water dispersion is k = ω²/g; finite-depth dispersion is ω² = gk tanh(kh). The wave number k is in radians per metre, depth h is in metres and g is the gravitational acceleration defined by the module. Finite-depth wave number is solved by bisection. Invalid depth can select deep water rather than reject the input; pre-validation remains necessary.</p>
<p>The derived rigid-body vertical displacement is z = heave − rₓ pitch + rᵧ roll, with angular terms converted to radians and horizontal offsets rₓ and rᵧ in metres. This small-angle extension is outside the computed example. The reported significant amplitude is twice the population standard deviation of the finite response record; it is not a maximum or an acceptance threshold.</p>
<p>For a narrow-band random process, the significant height relation is Hs = 4√m₀ and significant amplitude is Hs/2 = 2√m₀ = 2σ, where m₀ is the integral of the variance spectrum and σ² = m₀; see section 9.3. The present deterministic example evaluates 2σ from its finite sampled record. It does not estimate the highest-third wave statistic from a random sea.</p>
<p>The example is reconstructed through the public wave, analytical response and reconstruction interfaces. The signed history is checked separately against its closed-form cosine. The supplied interval is a software boundary, not a measured predictable zone. The bandwidth coherence proxy and aperture-based horizon functions require separate physical qualification.</p>''')
    body += section(6, 'Detailed results', f'''
<p>The regular-wave heave history is calculated from the registered inputs. Its sampled peak absolute displacement is {peak} m and its finite-record significant amplitude is {significant} m; neither value is compared with an equipment limit because none is supplied.</p>
<figure><svg id="motion-chart" viewBox="0 0 900 310" role="img" aria-label="Synthetic regular-wave heave against time with forecast interval boundary">
<path id="curve" fill="none" stroke="#137e8a" stroke-width="2"/>
<line x1="840" x2="840" y1="25" y2="250" stroke="#b45b20" stroke-dasharray="5 4"/>
<line x1="60" x2="840" y1="140" y2="140" stroke="#516573"/>
<text x="60" y="20">Heave (m)</text><text id="upper-scale" x="5" y="55"/><text id="lower-scale" x="5" y="235"/><text x="360" y="285">Time (s)</text>
<text x="530" y="45">Forecast interval boundary</text><text id="end-time" x="810" y="275"/>
<circle id="cursor" r="5" fill="#183449"/><title>Regular-wave heave; no operating limit supplied.</title></svg>
<label for="sample">Inspect time sample</label><input id="sample" type="range" min="0" value="0">
<p id="hover" aria-live="polite"></p>
<figcaption>Figure 6.1. Synthetic heave history. The dashed line marks the supplied time boundary, not an operating limit. The inspection control reports demand, units and the unavailable equipment limit.</figcaption></figure>
<p>No combined installation envelope or component capacity comparison is established. A monochromatic heave example supplies neither directional sea-state coverage nor equipment demand limits.</p>''')
    body += section(7, 'Validation evidence', '''
<p>The repository holds analytical checks and software regressions. Their evidence supports the stated identities and interfaces, not field prediction accuracy.</p>
<p>Regular-wave tests check signed amplitude and phase, reference-to-response spatial phase and heading projection. Finite-depth tests check the dispersion residual and the deep-water lower bound. Lever-arm tests check small-angle kinematics. Spectral tests compare component energy with spectral integration and a long realized record; the bookkeeping comparison shares spectrum and transfer definitions and is not an independent physical benchmark.</p>
<p>The response-file fixture is hand-authored adapter data, not independent solver validation. Measured-input tests use synthetic histories or local comma-separated-value files. A matched field record with phase-resolved waves, response measurements, equipment geometry and predeclared forecast-error criteria is not established by these tests. Current check results are recorded with the accompanying delivery record; historical pass counts are not represented as current execution evidence.</p>''')
    body += section(8, 'Findings, limitations and recommendations', '''
<p>The implementation provides a deterministic linear response calculation. Installation readiness remains conditional on evidence absent from the example.</p>
<h3>8.1 Limits of use and recommendations</h3>
<p>Restrict interpretation to linear wave response and small-angle rigid-body kinematics. Qualify incident phases and forecast lead-time error against independent measurements before operational use. Keep times inside the supplied interval; extrapolation is rejected but the interval itself is not qualified by that rejection.</p>
<p>Validate frequency, amplitude, depth, heading and sampling inputs before reconstruction. Grid response interpolation clamps outside its axes and does not wrap headings periodically; reject uncovered frequencies and headings during input preparation. Resolve the source-specific phase-sign uncertainty against matched physical response before using imported response data. These controls follow the implementation boundaries listed in section 9.2.</p>
<p>Supply an equipment-specific engineering basis for caution and limit values before issuing a go/no-go recommendation. Default criteria are indicative and lack edition-and-clause provenance in the inspected package; no default is adopted here. Nonlinear waves, forward-speed encounter effects and structural flexibility require an applicable model and separate validation.</p>
<h3>8.2 Not covered</h3>
<p>Directional spectrum generation, measured-motion reconciliation, affine correction, forecast-skill aggregation, operability fractions and lumped top-tension screening are catalogued but not demonstrated. Radar or lidar wave reconstruction and live sensor ingestion are not implemented by the synthetic sources. The lumped tension screen does not resolve distributed dynamics or snatch impact after slack. No licensed solver is used.</p>''')
    sources = {
        'Core definitions and dispersion': 'src/digitalmodel/motion_forecast/reconstruct.py',
        'Wave and motion models': 'src/digitalmodel/motion_forecast/models.py',
        'Response adapters and boundary behaviour': 'src/digitalmodel/motion_forecast/rao_adapter.py',
        'Phase conventions and outstanding source-sign qualification': 'src/digitalmodel/motion_forecast/conventions.py',
        'Public regular-wave example': EXAMPLE,
        'Spectral calculation checks': 'tests/motion_forecast/test_validation.py',
        'Synthetic source and horizon definitions': 'src/digitalmodel/motion_forecast/wave_forecast.py',
        'Indicative criteria': 'src/digitalmodel/motion_forecast/criteria_defaults.yml',
        'Capability and evidence inventory': 'src/digitalmodel/motion_forecast/docs/agent-pack.md',
    }
    refs = ''.join(f'<li>{label}: <a href="../../../../{path}">{path}</a>; '
                   f'content digest <code>{hashlib.sha256((ROOT / path).read_bytes()).hexdigest()}</code>.</li>'
                   for label, path in sources.items())
    body += section(9, 'References', f'''
<p>The numerical basis is the public repository example and implemented definitions. No external experimental dataset or licensed tabulation is reproduced.</p>
<h3>9.1 Evidence scope</h3><p>Repository content digests identify the inspected edition of each source. The example is an analytical software reference, not a published physical benchmark. No external standard limit is adopted.</p>
<h3>9.2 Source register</h3><ul>{refs}</ul>
<h3>9.3 Significant-amplitude definition</h3>
<p><a href="https://ocw.mit.edu/courses/2-019-design-of-ocean-systems-spring-2011/641e13d0b33fd958416245953423c5f3_MIT2_019S11_OWE.pdf#page=15">MIT OpenCourseWare, 2.019 Design of Ocean Systems, Spring 2011, lecture 9: Ocean Wave Environment, slide 15</a> gives the narrow-band relations for significant height and amplitude. The relation supplies the statistical definition, not an operational acceptance criterion.</p>''')
    body += section(10, 'Appendix: data register and quality evidence', '''
<p>The example is retained as a computed numerical payload within this document. Unknown engineering inputs are distinguished from true zero responses defined by the analytical adapter.</p>
<h3>10.1 Data register</h3>
<table><thead><tr><th>Data</th><th>Readiness</th><th>Required evidence</th></tr></thead><tbody>
<tr><td>Regular-wave example inputs</td><td>Synthetic; calculation demonstration only.</td><td>Table 4.1 and section 9.2.</td></tr>
<tr><td>Independent measured forecast accuracy</td><td>NOT_EVALUATED.</td><td>Time-aligned wave and motion records; lead-time error criterion.</td></tr>
<tr><td>Installation geometry and response data</td><td>NOT_EVALUATED.</td><td>Drawings, response axes, units, headings and phase qualification.</td></tr>
<tr><td>Equipment caution and operating limits</td><td>NOT_EVALUATED.</td><td>Applicable design basis, edition, clause and operation-specific approval.</td></tr>
</tbody><caption>Table 10.1. Data readiness and outstanding engineering evidence. No engineering assumption is substituted for missing acceptance data.</caption></table>
<h3>10.2 Quality pre-flight</h3>
<p>The rebuild test compares the embedded history and reported metrics with fresh public-interface calls, checks the signed closed-form solution and independent finite-record statistics, and requires byte-identical regenerated documents. Serialized histories and metrics are rounded to twelve decimal places; text output uses UTF-8 and LF line endings. Numerical metrics are formatted from calculated values. Source digests bind the description to the cited content. The chart is self-contained and uses the same calculated arrays. Browser interaction and file-saving behaviour require browser confirmation; static checks alone do not establish those behaviours.</p>
<details><summary>Engineering changes</summary><ul id="change-list"></ul></details>
<details><summary>Document comments</summary><p>Comments are saved as a separate file beside this document after a directory is selected in a browser supporting directory access. Existing comments are read before another is appended. A download is available when directory access is unavailable; place that file beside this document.</p>
<label for="comment">Comment and section</label><textarea id="comment" rows="4"></textarea>
<button id="save-comment" type="button">Save beside document</button>
<button id="download-comments" type="button">Download comments</button><p id="comment-status" aria-live="polite"></p></details>''')
    titles = re.findall(r'<h2>(.*?)</h2>', body)
    nav = ''.join(f'<li><a href="#section-{i}">{title}</a></li>' for i, title in enumerate(titles, 1))
    script = r'''
const d = JSON.parse(document.getElementById('example-data').textContent);
const c = JSON.parse(document.getElementById('change-data').textContent);
const scale = Math.max(...d.heave.map(Math.abs));
document.getElementById('upper-scale').textContent=scale.toFixed(3);
document.getElementById('lower-scale').textContent='−'+scale.toFixed(3);
const px = i => 60 + 780 * (d.time[i]-d.time[0]) / d.inputs.horizon;
const py = i => 140 - 90 * d.heave[i] / scale;
document.getElementById('curve').setAttribute('d', d.time.map((t,i)=>(i?'L':'M')+px(i)+','+py(i)).join(' '));
document.getElementById('end-time').textContent=d.inputs.horizon.toFixed(1);
const sample=document.getElementById('sample'); sample.max=d.time.length-1;
function inspect(){const i=Number(sample.value);const dot=document.getElementById('cursor');
dot.setAttribute('cx',px(i));dot.setAttribute('cy',py(i));
document.getElementById('hover').textContent='Regular wave; heave; time '+d.time[i].toFixed(1)+' s; demand '+d.heave[i].toFixed(3)+' m; equipment limit NOT_EVALUATED; interval boundary '+d.inputs.horizon.toFixed(1)+' s.';}
sample.addEventListener('input',inspect);inspect();
document.getElementById('motion-chart').addEventListener('pointermove',e=>{
const svg=e.currentTarget;const point=svg.createSVGPoint();point.x=e.clientX;point.y=e.clientY;
const x=point.matrixTransform(svg.getScreenCTM().inverse()).x;
sample.value=Math.round(Math.max(0,Math.min(1,(x-60)/780))*(d.time.length-1));inspect();});
c.changes.forEach(text=>{const li=document.createElement('li');li.textContent=text;document.getElementById('change-list').append(li);});
let comments=[];
const entry=()=>({sectionComment:document.getElementById('comment').value.trim()});
const status=text=>document.getElementById('comment-status').textContent=text;
document.getElementById('save-comment').addEventListener('click',async()=>{
try{if(!entry().sectionComment){status('Enter a comment first.');return;}
if(!window.showDirectoryPicker){status('Directory access is unavailable; use Download comments.');return;}
const directory=await window.showDirectoryPicker({mode:'readwrite'});
let existing=[];try{const old=await directory.getFileHandle('comments.json');existing=JSON.parse(await (await old.getFile()).text());
if(!Array.isArray(existing))throw new Error('Invalid comment file');}catch(error){if(error.name!=='NotFoundError')throw error;}
const updated=[...existing,entry()];const file=await directory.getFileHandle('comments.json',{create:true});
const writable=await file.createWritable();await writable.write(JSON.stringify(updated,null,2));await writable.close();
comments=updated;status('Comments saved in the selected directory.');}catch(error){status('Comments were not saved: '+error.message);}});
document.getElementById('download-comments').addEventListener('click',()=>{
if(!entry().sectionComment){status('Enter a comment first.');return;}
comments.push(entry());const url=URL.createObjectURL(new Blob([JSON.stringify(comments,null,2)],{type:'application/json'}));
const a=document.createElement('a');a.href=url;a.download='comments.json';a.click();URL.revokeObjectURL(url);
status('Downloaded comments require placement beside the document.');});
'''
    page = f'''<!DOCTYPE html>
<html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">
<title>Motion forecast module analysis</title>{style}</head><body class="hs-doc">
<header><h1>Motion forecast module analysis</h1><p>Draft for maintainer review</p></header>
<div class="layout"><nav aria-label="Contents"><ol>{nav}</ol></nav><main>{body}</main></div>
<script id="example-data" type="application/json">{json.dumps(data, allow_nan=False)}</script>
<script id="change-data" type="application/json">{json.dumps(changes)}</script>
<script>{script}</script></body></html>
'''
    destination = Path(destination)
    destination.mkdir(parents=True, exist_ok=True)
    (destination / 'index.html').write_text(page, encoding='utf-8', newline='\n')
    (destination / 'changes.json').write_text(json.dumps(changes, indent=2) + '\n', encoding='utf-8', newline='\n')


if __name__ == '__main__':
    build(Path(sys.argv[1]) if len(sys.argv) > 1 else DEST)
