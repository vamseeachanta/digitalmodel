"""Build the self-contained spectral hydrodynamics module analysis.

Run with the repository source importable and NumPy/SciPy installed:
    PYTHONPATH=src python docs/reports/modules/hydrodynamics/generate.py
No solver is started. The public test example supplies the input data.
"""
import ast
import hashlib
import inspect
import json
import re
from html import escape
from pathlib import Path

import numpy as np

from digitalmodel.hydrodynamics.seakeeping import operability_analysis
from digitalmodel.hydrodynamics.wave_spectra import WaveSpectra

ROOT = Path(__file__).resolve().parents[4]
HERE = Path(__file__).resolve().parent
EXAMPLE = 'tests/hydrodynamics/test_seakeeping.py'
STYLE = 'docs/api/cfd/turbulent-flat-plate-verification.html'
SOURCES = [EXAMPLE, 'src/digitalmodel/hydrodynamics/seakeeping.py',
           'src/digitalmodel/hydrodynamics/wave_spectra.py',
           'docs/api/_assets/brand.css', STYLE]


def example_inputs():
    """Read literal example inputs without importing or executing the test file."""
    tree = ast.parse((ROOT / EXAMPLE).read_text(encoding='utf-8'))
    cls = next(n for n in tree.body if isinstance(n, ast.ClassDef)
               and n.name == 'TestOperabilityAnalysis')
    methods = {n.name: n for n in cls.body if isinstance(n, ast.FunctionDef)}
    scatter = ast.literal_eval(next(n.value for n in ast.walk(methods['scatter_diagram'])
                                    if isinstance(n, ast.Return)))
    grid = next(n for n in ast.walk(methods['unity_raos'])
                if isinstance(n, ast.Call) and isinstance(n.func, ast.Attribute)
                and n.func.attr == 'linspace')
    omega = np.linspace(*(ast.literal_eval(n) for n in grid.args))
    criterion = next(ast.literal_eval(n.value)['heave']
                     for n in ast.walk(methods['test_result_keys'])
                     if isinstance(n, ast.Assign) and isinstance(n.value, ast.Dict))
    return omega, np.ones_like(omega), scatter, criterion


def calculate():
    """Call the public module and retain its inputs and native return values."""
    omega, rao, scatter, criterion = example_inputs()
    defaults = inspect.signature(WaveSpectra.jonswap).parameters
    return {
        'inputs': {'omega': omega.tolist(), 'rao': rao.tolist(), 'scatter': scatter,
                   'criterion': criterion, 'gamma': defaults['gamma'].default},
        'result': operability_analysis(omega, rao, scatter, {'heave': criterion}),
        'source_digest_format': 'sha256-utf8-lf',
        'sources': {name: hashlib.sha256(
                        (ROOT / name).read_text(encoding='utf-8').encode('utf-8')).hexdigest()
                    for name in SOURCES},
    }


def number(value, key=""):
    """Use consistent engineering precision without rounding small values to zero."""
    if key.endswith('-m0'):
        return f'{value:.3f}'
    if key == 'points':
        return str(int(value))
    if value != 0 and abs(value) < 0.05:
        return f'{value:.1e}'
    return f'{value:.1f}'


def display_values(data):
    """Return the complete numeric register for the worked-example table and prose."""
    inputs = data['inputs']
    values = {'criterion': inputs['criterion'], 'gamma': inputs['gamma'],
              'minimum': inputs['omega'][0], 'maximum': inputs['omega'][-1],
              'points': len(inputs['omega']), 'rao': inputs['rao'][0],
              'operability': data['result']['operability_pct']}
    for i, row in enumerate(data['result']['sea_state_results']):
        for key in ('hs', 'tp', 'probability', 'm0', 'significant_amplitude'):
            values[f'{i}-{key}'] = row[key]
    return values


def chart(data):
    """Draw a demand-versus-wave-height chart with accessible native hover detail."""
    rows = data['result']['sea_state_results']
    limit = data['inputs']['criterion']
    xmax = max(r['hs'] for r in rows) * 1.15
    ymax = max(limit, *(r['significant_amplitude'] for r in rows)) * 1.3
    x = lambda v: 70 + 560 * v / xmax
    y = lambda v: 290 - 230 * v / ymax
    parts = ['<svg viewBox="0 0 700 350" role="img" aria-label="Heave demand and example limit">',
             '<path d="M70 50V290H650" fill="none" stroke="currentColor"/>',
             '<text x="235" y="335">Significant wave height (m)</text>',
             '<text x="12" y="30">Significant single amplitude (m)</text>',
             f'<path d="M70 {y(limit)}H650" stroke="#b45309" stroke-dasharray="6 4"/>',
             f'<text x="75" y="{y(limit)-10}">Example limit: {number(limit)} m</text>']
    for row in rows:
        hs, demand = row['hs'], row['significant_amplitude']
        label = (f"Public synthetic case; heave; wave height {number(hs)} m; "
                 f"demand {number(demand)} m; limit {number(limit)} m")
        parts.append(f'<circle tabindex="0" cx="{x(hs)}" cy="{y(demand)}" r="7" '
                     f'fill="#137e8a" data-demand="{demand}" data-limit="{limit}">'
                     f'<title>{label}</title></circle>')
        parts.append(f'<text x="{x(hs)-5}" y="310">{number(hs)}</text>')
        parts.append(f'<text x="{x(hs)+10}" y="{y(demand)-8}">{number(demand)} m</text>')
    return ''.join(parts) + '</svg>'


REVIEW = r"""
<aside id="changes-panel"><h3>Changes since the previous revision</h3><p id="changes-text"></p></aside>
<aside aria-label="Review comments"><h3>Review comments</h3>
<p>Comments are retained in this browser. Save beside this report as comments.json;
use Import comments to restore a saved copy. Where direct file saving is unavailable,
the browser downloads a copy for placement beside the report.</p>
<label>Section <select id="comment-section"></select></label>
<label>Comment <textarea id="comment-text" rows="3"></textarea></label>
<button id="add-comment" type="button">Add comment</button>
<button id="save-comments" type="button">Save comments</button>
<label>Import comments <input id="import-comments" type="file" accept="application/json"></label>
<p id="comment-status" role="status"></p><ol id="comment-list"></ol></aside>
<script>
(() => {
  const key = 'hydrodynamics-module-comments-v1';
  const byId = id => document.getElementById(id);
  let comments = [];
  const valid = rows => Array.isArray(rows) && rows.every(row =>
    typeof row.section === 'string' && typeof row.text === 'string');
  const status = text => { byId('comment-status').textContent = text; };
  try { const saved = JSON.parse(localStorage.getItem(key) || '[]');
    if (valid(saved)) comments = saved;
  } catch (_) { status('Browser storage is unavailable; export comments before closing.'); }
  const draw = () => {
    byId('comment-list').replaceChildren();
    comments.forEach(row => { const li = document.createElement('li');
      li.textContent = row.section + ': ' + row.text; byId('comment-list').append(li); });
    try { localStorage.setItem(key, JSON.stringify(comments)); }
    catch (_) { status('Browser storage is unavailable; export comments before closing.'); }
  };
  document.querySelectorAll('h2').forEach(heading => {
    const option = document.createElement('option'); option.textContent = heading.textContent;
    byId('comment-section').append(option);
  });
  byId('add-comment').onclick = () => {
    const text = byId('comment-text').value.trim(); if (!text) return;
    comments.push({section: byId('comment-section').value, text});
    byId('comment-text').value = ''; draw();
  };
  byId('save-comments').onclick = async () => {
    const text = JSON.stringify(comments, null, 2);
    try {
      if (window.showSaveFilePicker) {
        const handle = await window.showSaveFilePicker({suggestedName:'comments.json'});
        const stream = await handle.createWritable(); await stream.write(text); await stream.close();
      } else {
        const url = URL.createObjectURL(new Blob([text], {type:'application/json'}));
        const a = document.createElement('a'); a.href = url; a.download = 'comments.json'; a.click();
        URL.revokeObjectURL(url);
      }
      status('Comments exported. Retain the file beside the report.');
    } catch (_) { status('Comments were not exported. Retry Save comments.'); }
  };
  byId('import-comments').onchange = async event => {
    try { const rows = JSON.parse(await event.target.files[0].text());
      if (!valid(rows)) throw new Error('Invalid comments'); comments = rows; draw();
    } catch (_) { status('The selected file is not a comments array.'); }
  };
  const changes = JSON.parse(byId('changes-data').textContent);
  byId('changes-text').textContent = changes[changes.length - 1].description;
  draw();
})();
</script>
"""


def render(data, changes):
    """Write the complete engineering narrative with calculated value bindings."""
    values = display_values(data)
    def value(key):
        return f'<span data-value="{key}">{number(values[key], key)}</span>'
    rows = []
    for i, row in enumerate(data['result']['sea_state_results']):
        cells = ''.join(f'<td>{value(f"{i}-{key}")}</td>' for key in
                        ('hs', 'tp', 'probability', 'm0', 'significant_amplitude'))
        disposition = 'The example limit is met.' if row['operable'] else 'The example limit is exceeded.'
        rows.append(f'<tr>{cells}<td>{disposition}</td><td><a href="#basis-example">Example basis</a></td></tr>')
    style_text = (ROOT / STYLE).read_text(encoding='utf-8')
    style = re.search(r'<style>.*?</style>', style_text, re.S)[0]
    brand = (ROOT / 'docs/api/_assets/brand.css').read_text(encoding='utf-8')
    brand = re.sub(r'/\*.*?\*/', '', brand, flags=re.S)
    source_rows = ''.join(f'<li><a href="../../../../{escape(path)}">{escape(path)}</a></li>'
                          for path in SOURCES)
    return f'''<!DOCTYPE html>
<html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">
<title>Hydrodynamics module analysis — spectral motion and operability</title>
<style>{brand}</style>{style}
</head><body class="hs-doc"><div class="wrap">
<header><h1>Hydrodynamics module analysis</h1><p>Spectral motion and operability</p>
<p>Draft for client review</p></header>
<main>
<section id="summary"><h2>1. Executive Summary</h2>
<p>The hydrodynamics module is a collection of wave, motion and hydrodynamic loading calculations.
Analysis is performed to determine the capabilities and limits of its spectral motion workflow;
key results are summarized below.</p>
<ul><li>Use the spectral workflow to calculate motion from a supplied response function.
<ul><li>The public synthetic example returns {value('operability')} % weighted operability against
an example significant single-amplitude limit of {value('criterion')} m.</li></ul></li>
<li>Keep the demonstration separate from an operating decision.
<ul><li>The example uses a unity response function and synthetic sea-state probabilities.
Measured response data and an approved motion criterion are not established.</li></ul></li></ul>
</section>
<div id="revision-history"><h3>Revision history</h3>
<table><thead><tr><th>Revision</th><th>Date</th><th>Change and reason</th></tr></thead>
<tbody><tr><td>Initial</td><td>2026-10-21</td><td>The spectral workflow is documented to distinguish implemented calculations from operational evidence.</td></tr>
<tr><td>1</td><td>2026-10-22</td><td>Source hashes use canonical text and generated files use LF so checkout line endings do not change the report.</td></tr></tbody>
<caption>Revision history.</caption></table></div>
<section id="introduction"><h2>2. Introduction</h2>
<p>The purpose, scope and status of the analysis are presented in this section.
The main workflow is assessed from a wave spectrum and a response amplitude operator through
motion variance, significant amplitude and weighted operability.</p>
<p>A response amplitude operator is the motion amplitude per unit incident wave amplitude at a
specified frequency and heading. The present example represents heave, the vertical translation
of a floating body. Public callable functions and existing tests are present, so the module is
reported as an implemented workflow rather than as a readiness proposal.</p>
<h3>Not covered</h3><p>Coefficient storage and interpolation, environmental loading, diffraction adapters,
hull geometry, passing-body interaction, propulsion, river currents, tank sloshing and response
file conversion are outside the calculations assessed here. Resistance, effective power, trim
sensitivity, appendage sensitivity and computational fluid dynamics are not evaluated.</p>
</section>
<section id="data"><h2>3. Design Data</h2>
<p>The input basis is taken from the public synthetic operability example identified in
<a href="#locators">References</a>. No hull dimensions, density, viscosity, draft or speed
are supplied; no physical hull or mesh is inferred.</p>
<p id="basis-example">The angular-frequency grid extends from {value('minimum')} to
{value('maximum')} rad/s with {value('points')} equally spaced samples. The heave response
amplitude ratio is {value('rao')} m/m throughout the band. The spectrum peakedness input is
{value('gamma')}, taken from the function default. The example criterion is
{value('criterion')} m, taken from the existing example assertion setup; it is not a sourced operating limit.</p>
<figure><svg viewBox="0 0 760 210" role="img" aria-label="Conceptual heave response model">
<path d="M20 95Q50 70 80 95T140 95T200 95T260 95T320 95T380 95T440 95T500 95T560 95T620 95T680 95" fill="none" stroke="#137e8a"/>
<path d="M270 65H460L435 130H295Z" fill="#e6eef3" stroke="#183449"/>
<path d="M365 40V160M355 50L365 40L375 50M355 150L365 160L375 150" fill="none" stroke="#183449"/>
<text x="30" y="40">Incident irregular waves</text><text x="490" y="155">Heave response</text>
<text x="25" y="195">Conceptual model; no hull geometry, mooring or mesh is specified.</text>
</svg><figcaption>Figure A. Spectral heave model and incident-wave boundary input.</figcaption></figure>
<p>The input arrays use angular frequency in rad/s. Wave spectral density has units m²·s.
The response function is real amplitude in m/m for this example. A rotational response requires
its own angular units; angular and translational criteria are not interchangeable.</p>
</section>
<section id="method"><h2>4. Analysis Methodology</h2>
<p>Linear response is calculated on a common frequency grid. The equations below describe the
implemented methods identified in <a href="#locators">References</a>; no standard certification
or measured-response validation is implied.</p>
<p>Sᵣ(ω) = |R(ω)|² Sη(ω), where ω is angular frequency, R is the real response amplitude ratio,
Sη is incident-wave spectral density and Sᵣ is response spectral density.
The response function is squared point by point by the response-spectrum function.</p>
<p>mₙ = ∫ ωⁿ Sᵣ(ω) dω, evaluated over the supplied band by trapezoidal integration.
The symbol n is the moment order and m₀ is the response variance.
The significant single amplitude is aₛ = 2√m₀. This is not the crest-to-trough significant height.</p>
<p>For narrow-band Gaussian response with Rayleigh-distributed peaks,
P(A &gt; a) = exp(−a²/(2m₀)), where A is peak amplitude and a is the threshold.
This probability applies to a peak, not to exceedance during an exposure duration.</p>
<p>The wave-spectrum generator normalizes the discrete spectral area to Hₛ²/16,
where Hₛ is significant wave height. Operability is calculated as
100 Σ pᵢ χ(aₛ,ᵢ ≤ aₗᵢₘ)/Σ pᵢ, where pᵢ is sea-state occurrence weight,
χ is an indicator, the subscript identifies a sea state and aₗᵢₘ is the supplied criterion.</p>
<h3>Basis and criteria</h3><p>The worked example is compared with its supplied amplitude criterion because
that is the comparison performed by the public operability function. No external marine-operation
criterion is substituted. Source-code equations and the analytical tests constitute the calculation
basis; the test fixture constitutes the input basis. Published experimental validation for this
specific example is NOT_EVALUATED.</p>
<p>Model-to-full-scale resistance conversion and wall treatment are NOT_EVALUATED because this
workflow evaluates supplied frequency-domain motion rather than a discretized viscous flow field.</p>
</section>
<section id="results"><h2>5. Detailed Results</h2>
<p>The module is called directly with the documented synthetic inputs. The returned sea-state
results are retained without replacing the calculation by an independent report formula.</p>
<p>The significant single-amplitude demands are compared with the example criterion in Figure B.
Hover over or focus a marker to read the case, component, demand, limit and units.</p>
<figure>{chart(data)}<figcaption>Figure B. Heave demand against significant wave height; the dashed line is the example criterion.</figcaption></figure>
<p>The native sea-state results are listed in <a href="#worked-table">Table A in Appendix C</a>.</p>
<p>The returned weighted operability is {value('operability')} % against the example criterion
of {value('criterion')} m. The highest-wave-height case exceeds that criterion.
This result describes only the supplied response function, frequency band and sea-state weights.</p>
<h3>Existing validation evidence</h3><p>The existing seakeeping tests check identity response, zero response,
response-amplitude scaling, mismatched array lengths, integration of a flat spectrum,
wave-height recovery, known significant amplitude, Rayleigh probability and operability extremes.
These are analytical and software consistency checks. Wave-height recovery is partly enforced by
discrete normalization and therefore is not independent evidence for spectral shape accuracy.</p>
<p>Physical response accuracy, directional spreading, measured response amplitude operators,
full-scale operability and time-domain extremes are NOT_EVALUATED in this report.</p>
</section>
<section id="findings"><h2>6. Key Findings</h2>
<p>The calculation supports a limited software-use conclusion. The distinction between an example
criterion and an operating limit governs interpretation.</p>
<ul><li>The spectral workflow is callable without a flow solver.
<ul><li>Arrays, motion variance and sea-state dispositions are returned for further assessment.</li></ul></li>
<li>The example exceeds its criterion in the highest-wave-height case.
<ul><li>The weighted percentage is conditional on synthetic weights and a unity response function.</li></ul></li></ul>
</section>
<section id="limits"><h2>7. Limitations</h2>
<p>The applicability limits are determined from the implementation and the evidence basis.
Uncertainty in physical response cannot be quantified from the present example.</p>
<p>Only the first dictionary criterion is applied by the operability function.
Simultaneous motion-component limits are not evaluated.</p>
<p>The operability function creates a uniform spectrum grid from the endpoint frequencies and
sample count. Nonuniform input response grids are therefore not matched point by point.</p>
<p>The response-spectrum function squares real input amplitudes. Complex response functions are
not converted to modulus squared by that function; supply real nonnegative amplitudes.</p>
<p>Finite, increasing frequencies, consistent array shapes, positive periods, nonnegative spectral
densities and meaningful motion criteria require caller checks. The functions do not comprehensively
reject invalid inputs. Sea-state weights are normalized by their sum; empty or zero-total weights
return zero operability without establishing a probability model.</p>
<p>The Rayleigh approximation requires narrow-band Gaussian response. Its applicability to
broad-band, nonlinear or impact-dominated motions is not established. Positive variance and a
nonnegative amplitude threshold are required for physical interpretation.</p>
</section>
<section id="recommendations"><h2>8. Recommendations</h2>
<p>The following actions are tied to the implementation limits and the missing engineering basis.
The example is retained as a software demonstration.</p>
<ul><li>Exclude the failing synthetic case from the example operating envelope at the present
criterion. For an actual assessment, obtain a heading-specific response function and an approved
motion limit before assigning an operating envelope.</li>
<li>Validate finite ascending uniform frequencies, response units, array alignment and positive
total sea-state weight before calling operability. Reject unsupported input rather than infer it.</li>
<li>Evaluate each required motion component separately. Combine simultaneous criteria externally
until the interface supports joint sea-state assessment.</li>
<li>Establish grid and bandwidth convergence against an independent analytical or published
response benchmark. Height recovery alone is insufficient because the spectrum is normalized.</li>
<li>Obtain exposure duration and peak statistics before converting peak exceedance to an
operation-duration probability. The implemented peak formula does not supply that conversion.</li></ul>
</section>
<section id="references"><h2>9. References</h2>
<p>The calculation basis is restricted to the public repository records below.
No numerical value from a licensed publication is reproduced.</p>
<h3>9.1 Source basis</h3><p>The seakeeping implementation, public analytical tests and spectrum implementation
form the source edition represented by this report. SHA-256 source digests are embedded in the report's
numeric attachment with the format sha256-utf8-lf: UTF-8 text with CRLF and CR line endings
normalized to LF before hashing. These bind source text independently of checkout line endings.
The implementation cites offshore hydromechanics literature, but editions and
clauses are not available in the assessed example; those citations are not adopted as verified sources.</p>
<h3 id="locators">9.2 Repository records</h3><ul>{source_rows}</ul>
<p>The public entry point is called with the documented arrays and criterion as follows.</p>
<pre><code>from digitalmodel.hydrodynamics.seakeeping import operability_analysis
result = operability_analysis(
    rao_freqs=omega,
    rao_amplitudes=heave_amplitudes,
    scatter_diagram=sea_states,
    criteria={{"heave": significant_amplitude_limit}},
)</code></pre>
<p>The return mapping contains operability_pct and sea_state_results. Each sea-state record
contains hs, tp, probability, m0, significant_amplitude and operable. The array values and native
return mapping used here are embedded in this single-file document.</p>
</section>
<section id="symbols"><h2>10. Acronyms and Symbols</h2>
<p>The notation is defined for the spectral heave calculation. No response solver acronym is
required to interpret the worked example.</p>
<p>RAO means response amplitude operator. Hₛ denotes significant wave height; Tₚ denotes peak period;
γ denotes spectral peakedness; ω denotes angular frequency; R denotes the response amplitude ratio;
Sη and Sᵣ denote wave and response spectral densities; mₙ denotes a spectral moment; aₛ denotes
significant single amplitude; aₗᵢₘ denotes its criterion; p denotes occurrence weight.</p>
<h3>Appendix A. Data register</h3><table><thead><tr><th>Item</th><th>Basis</th><th>Intended use</th></tr></thead>
<tbody><tr><td>Sea states, response ratio and criterion</td><td>ASSUMED - to be confirmed for any physical application; public synthetic example</td><td>Software demonstration only</td></tr>
<tr><td>Spectrum coefficients</td><td>Existing implementation; independent source qualification is NOT_EVALUATED</td><td>Reproduction of implemented behaviour only</td></tr>
<tr><td>Hull, heading and measured response</td><td>Not supplied</td><td>Required before operational use</td></tr></tbody>
<caption>Table B. Data basis and unresolved inputs.</caption></table>
<h3>Appendix B. Quality evidence</h3><p>The report rebuild checks compare the embedded native return values
with direct public-function calls and compare every bound table and prose number with the calculation
register. Source digests bind the report to its input records. The chart uses the same native demand
and criterion values as the table. No flow mesh or licensed solver is used.</p>
<p>Resistance coefficients, mesh evidence, trim investigations, appendage sensitivity, wall treatment
and divergent cells are NOT_EVALUATED because no resistance or flow solution is produced.</p>
<h3>Appendix C. Worked-example values</h3>
<table id="worked-table"><thead><tr><th>Wave height (m)</th><th>Peak period (s)</th><th>Occurrence weight (1)</th>
<th>Variance (m²)</th><th>Single amplitude (m)</th><th>Disposition</th><th>Source</th></tr></thead>
<tbody>{''.join(rows)}</tbody><caption>Table A. Native sea-state results from the public synthetic example.</caption></table>
</section>
<script id="example-data" type="application/json">{json.dumps(data, sort_keys=True, allow_nan=False)}</script>
<script id="changes-data" type="application/json">{json.dumps(changes, sort_keys=True)}</script>
{REVIEW}
</main></div></body></html>
'''


def build(output=HERE):
    """Rebuild both deliverables in the requested directory."""
    output = Path(output)
    output.mkdir(parents=True, exist_ok=True)
    changes = [{'revision': 'Initial', 'date': '2026-10-21',
                'description': 'The spectral motion workflow and its example criterion are documented; physical operating qualification remains unevaluated.'},
               {'revision': '1', 'date': '2026-10-22',
                'description': 'Source hashes use canonical UTF-8 text with LF line endings and generated files use LF so checkout line endings do not change the report.'}]
    data = calculate()
    page = '\n'.join(line.rstrip() for line in render(data, changes).splitlines()) + '\n'
    (output / 'index.html').write_text(page, encoding='utf-8', newline='\n')
    (output / 'changes.json').write_text(json.dumps(changes, indent=2) + '\n', encoding='utf-8', newline='\n')


if __name__ == '__main__':
    build()
