# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Build the RAO workflow analysis using public numerical entry points.

Reproduce with PYTHONPATH=src python scripts/reporting/build_rao_analysis_report.py
Use --output for a scratch rebuild. No network or licensed solver is accessed.
Synthetic example inputs are chosen for independent hand and quadrature checks;
no geometry-specific hydrodynamic prediction or operating limit is established.
"""

from __future__ import annotations

import argparse
import hashlib
from html import escape
import json
import math
import re
from pathlib import Path

import numpy as np

from digitalmodel.hydrodynamics.rao_analysis import regular_wave_response, spectral_response
from digitalmodel.hydrodynamics.wave_spectra import WaveSpectra

ROOT = Path(__file__).resolve().parents[2]
OUTPUT = ROOT / 'docs/reports/modules/rao_analysis'
SOURCES = {
    'rao': 'https://www.orcina.com/webhelp/OrcaFlex/Content/html/Vesseltheory,RAOsandphases.htm',
    'spectra': 'https://www.orcina.com/webhelp/OrcaFlex/Content/html/Waves,Wavespectra.htm',
    'parameters': 'https://www.orcina.com/webhelp/OrcaFlex/Content/html/Environment,DataforJONSWAPandISSCspectra.htm',
    'hydrostatics': 'https://www.wamit.com/manual7.x/html/wamit_v75manualch3.html',
    'bibliography': 'https://www.orcina.com/webhelp/OrcaFlex/Content/html/Referencesandlinks.htm#Tucker1991',
}
DEPENDENCIES = [
    'scripts/reporting/build_rao_analysis_report.py',
    'src/digitalmodel/hydrodynamics/rao_analysis/response.py',
    'src/digitalmodel/hydrodynamics/rao_analysis/__init__.py',
    'src/digitalmodel/hydrodynamics/seakeeping.py',
    'src/digitalmodel/hydrodynamics/wave_spectra.py',
    'src/digitalmodel/solvers/orcawave/hydrostatics.py',
    'src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py',
    'docs/reports/modules/rao_analysis/changes.json',
    'docs/api/_assets/brand.css',
    'src/digitalmodel/reporting/assets/calc_report_format.html',
]


def stats(result):
    """Retain integrated values from the actual public response result."""
    return {key: getattr(result, key) for key in ['m0', 'rms', 'significant_amplitude']}


def calculate(peak_gain=2):
    """Compute the synthetic example, tail diagnostic and figure samples.

    Input choices are arithmetic examples, not empirical coefficients. The
    heave-like shape is defined here as a Gaussian transfer peak and is not a
    derived vessel RAO. JONSWAP widths are those of the existing generator;
    their public source is Orcina, Data for JONSWAP and ISSC spectra.
    """
    inputs = dict(hs=4.0, tp=5.0, gamma=3.0, peak_gain=float(peak_gain),
                  peak_width=0.3, regular_rao=3.0, regular_wave_amplitude=2.0,
                  regular_omega=1.0, lower_peak_ratio=0.01, upper_peak_ratio=10.0,
                  grid_points=40001)
    wp = 2*np.pi/inputs['tp']
    omega, wave = WaveSpectra().jonswap(
        inputs['hs'], inputs['tp'], gamma=inputs['gamma'],
        freq_min=inputs['lower_peak_ratio']*wp,
        freq_max=inputs['upper_peak_ratio']*wp, n_points=inputs['grid_points'],
    )
    rao = peak_gain*np.exp(-((omega-wp)/inputs['peak_width'])**2)
    peak = spectral_response(omega, rao, wave)
    unit = spectral_response(omega, np.ones_like(omega), wave)
    time = np.linspace(0, 4*np.pi, 201)
    motion = regular_wave_response(
        [inputs['regular_omega']], [inputs['regular_rao']],
        [inputs['regular_wave_amplitude']], time,
    )
    # Public modified Pierson–Moskowitz formula: Waves in Ocean Engineering (1991), p107,
    # reproduced by Orcina, Waves: Wave spectra. No finite-grid normalization.
    pm = (5/16)*inputs['hs']**2*wp**4*omega**-5*np.exp(-(5/4)*(wp/omega)**4)
    pm_result = spectral_response(omega, np.ones_like(omega), pm)
    exact = inputs['hs']**2/16
    # Integration substitution u=(wp/w)^4: finite m0=Hs²/16*exp[-5/4(wp/W)^4].
    tail = dict(pm_m0=pm_result.m0, infinite_m0=exact,
                finite_exact=exact*math.exp(-(5/4)*(wp/omega[-1])**4),
                cutoff=2.0, upper_omega=float(omega[-1]), lower_omega=float(omega[0]))
    short = omega <= tail['cutoff']
    truncated = spectral_response(omega[short], np.ones(sum(short)), pm[short])
    tail['truncated_m0'] = truncated.m0
    tail['truncated_loss_pct'] = 100*(1-truncated.m0/exact)
    tail['wide_loss_pct'] = 100*(1-pm_result.m0/exact)
    # Round an upper bound upward, so the displayed comparison remains true.
    tail['wide_loss_bound_pct'] = math.ceil(tail['wide_loss_pct']*10)/10
    # 0.000125 analytic tail fraction plus <0.000001 quadrature error permits
    # 0.0002 relative tolerance. It is a numerical check, not design acceptance.
    tail['m0_relative_tolerance'] = 0.0002
    idx = np.arange(0, len(omega), 100)
    curves = {name: values[idx].tolist() for name, values in
              [('omega', omega), ('wave', wave), ('rao', rao), ('response', peak.spectrum)]}
    curves.update(time=time.tolist(), motion=motion.tolist())
    return dict(inputs=inputs, constant=dict(amplitude=float(np.max(motion))),
                unit=stats(unit), peak=stats(peak), tail=tail, curves=curves,
                source_digests={name: hashlib.sha256((ROOT/name).read_bytes()).hexdigest()
                                for name in DEPENDENCIES})


def curve_digest(values):
    """Identify the computed samples used by a figure."""
    return hashlib.sha256(json.dumps(values, separators=(',', ':')).encode()).hexdigest()


def format_value(value):
    """Display engineering values with one decimal and small values explicitly."""
    if isinstance(value, int):
        return str(value)
    return f'{value:.1e}' if value and abs(value) < 0.05 else f'{value:.1f}'


def number(data, key):
    """Bind visible text to a named computed or declared input value."""
    value = data
    for part in key.split('.'):
        value = value[part]
    return f'<span data-value="{key}">{format_value(value)}</span>'


def figure(data, name, x_name, series, xlabel, ylabel):
    """Generate every curve and axis from computed samples, including limits."""
    curves = data['curves']
    x = curves[x_name]
    ymin = min(0.0, min(min(curves[y]) for y, _ in series))
    ymax = max(max(curves[y]) for y, _ in series)
    left, right, top, bottom = 70, 760, 30, 270
    colors = ['#005a80', '#9a3d00']
    lines = []
    for (y, label), color in zip(series, colors):
        points = ' '.join(f'{left+(a-x[0])/(x[-1]-x[0])*(right-left):.1f},{bottom-(b-ymin)/(ymax-ymin)*(bottom-top):.1f}'
                          for a, b in zip(x, curves[y]))
        lines.append(f'<polyline data-curve="{y}" data-digest="{curve_digest(curves[y])}" '
                     f'fill="none" stroke="{color}" stroke-width="2" points="{points}"><title>{escape(label)}</title></polyline>')
    ticks = []
    for ratio in [0, 0.5, 1]:
        xtick = x[0]+ratio*(x[-1]-x[0])
        ticks.append(f'<text x="{left+ratio*(right-left):.1f}" y="295">{format_value(xtick)}</text>')
        ticks.append(f'<text x="5" y="{bottom-ratio*(bottom-top):.1f}">{format_value(ymin+ratio*(ymax-ymin))}</text>')
    legend = '; '.join(label for _, label in series)
    return (f'<figure id="figure-{name}"><svg viewBox="0 0 800 340" role="img" aria-label="{escape(legend)}" '
            f'data-x-digest="{curve_digest(x)}"><title>{escape(legend)}</title>'
            f'<path d="M{left},{top} V{bottom} H{right}" fill="none" stroke="currentColor"/>'
            + ''.join(lines+ticks) + f'<text x="300" y="330">{escape(xlabel)}</text>'
            + f'<text x="80" y="20">{escape(ylabel)}</text></svg><figcaption>{escape(legend)}. '
            'Curves are sampled from the calculation arrays; statistics use the full grid.</figcaption></figure>')


def render(data, changes):
    """Compose the report with the existing report style and computed figures."""
    def n(key):
        return number(data, key)
    brand = re.sub(r'/\*.*?\*/', '', (ROOT/'docs/api/_assets/brand.css').read_text(), flags=re.S)
    # Use the existing prose-report template's style, as in other module reports.
    template = (ROOT/'src/digitalmodel/reporting/assets/calc_report_format.html').read_text()
    styles = '\n'.join(re.findall(r'<style[^>]*>(.*?)</style>', template, re.S))
    rows = ''.join(f'<tr><td>{escape(key)}</td><td>{n("inputs."+key)}</td></tr>' for key in data['inputs'])
    result_rows = ''.join(f'<tr><td>{case}</td><td>{n(case+".m0")}</td><td>{n(case+".rms")}</td><td>{n(case+".significant_amplitude")}</td></tr>'
                          for case in ['unit', 'peak'])
    digests = ''.join(f'<tr><td>{escape(key)}</td><td><code>{value}</code></td></tr>'
                      for key, value in data['source_digests'].items())
    links = ''.join(f'<li><a href="{url}">{name}</a></li>' for name, url in SOURCES.items())
    revisions = f'<tr><td>{changes["revision"]}</td><td>{changes["date"]}</td><td>Initial computed RAO workflow analysis and limits.</td></tr>'
    change_list = ''.join(f'<li>{escape(item["description"])}</li>' for item in changes['changes'])
    regular = figure(data, 'regular', 'time', [('motion', 'Regular response')], 'Time (s)', 'Response (m)')
    spectra = figure(data, 'spectra', 'omega', [('wave', 'JONSWAP wave density'), ('response', 'Response density')], 'Angular frequency (rad/s)', 'Density (m² per rad/s)')
    rao = figure(data, 'rao', 'omega', [('rao', 'Synthetic heave-like RAO')], 'Angular frequency (rad/s)', 'RAO (m/m)')
    return f'''<!doctype html>
<html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">
<title>RAO response module analysis</title><style>{brand}\n{styles}
body{{max-width:1100px;margin:auto;padding:2rem}}table{{border-collapse:collapse;width:100%}}td,th{{padding:.5rem;border:1px solid #ccc;text-align:left}}svg{{width:100%;height:auto}}code{{overflow-wrap:anywhere}}section{{margin:2rem 0}}figure{{margin:1rem 0}}textarea{{width:100%}}
</style></head><body class="hs-doc"><main><h1>RAO response module analysis</h1>
<p>Draft analytical example. No operational clearance is established.</p>
<section id="summary"><h2>1 Summary</h2>
<p>The constant operator returns an amplitude of {n('constant.amplitude')} m from RAO {n('inputs.regular_rao')} m/m and wave amplitude {n('inputs.regular_wave_amplitude')} m.
The synthetic peak example gives variance {n('peak.m0')} m², RMS {n('peak.rms')} m and significant single amplitude {n('peak.significant_amplitude')} m.</p>
<p>The response contracts and independent arithmetic are evaluated. Geometry-specific motion prediction and operating acceptance are NOT_EVALUATED. Supply a qualified RAO and assess spectral convergence before applying the workflow.</p>
<div id="revision-history"><h3>Revision history</h3><table><thead><tr><th>Revision</th><th>Date</th><th>What changed and why</th></tr></thead><tbody>{revisions}</tbody></table></div></section>
<section id="scope"><h2>2 Scope and capability</h2>
<p>The numerical public entry points are <code>regular_wave_response</code> and <code>spectral_response</code> in the RAO-analysis package. The existing <code>RAOAnalysis</code> plotting class remains available. Legacy plotting scripts are not numerical response solvers.</p>
<p>This analysis covers regular-wave superposition, spectral transfer, finite-band statistics and explicit input contracts. Related module reports cover diffraction (#3222), spectral hydrodynamics (#3227) and OrcaWave spectral reporting (#3233).</p></section>
<section id="basis"><h2>3 Inputs and calculation basis</h2>
<p>Inputs below are synthetic arithmetic choices. The peak shape is assumed, based on previous experience, and is not a vessel prediction. Its width and gain are required to reproduce this example and imply no acceptance limit.</p>
<table><thead><tr><th>Input</th><th>Value</th></tr></thead><tbody>{rows}</tbody></table>
<p>Hs is metres, Tp seconds, regular omega and peak width rad/s, amplitudes metres, and RAOs m/m. Gamma and relative grid bounds are dimensionless. Grid points are a count. Bounds are {n('tail.lower_omega')} to {n('tail.upper_omega')} rad/s. The finite-grid spectrum generator normalizes its density to Hs²/16; this is declared rather than used as evidence that the tail is complete.</p></section>
<section id="method"><h2>4 Method and equation sources</h2>
<p>At a common phase origin, eta(t)=sum a_j cos(omega_j t+theta_j) and x(t)=sum R_j a_j cos(omega_j t+theta_j-phi_j) for lag phases. Positive lead uses +phi_j. Orcina's RAO theory defines the single-component equation; superposition follows from linearity.</p>
<p>Independent Fourier components give S_r(omega)=|R(omega)|² S_w(omega). Integrate over the supplied angular-frequency band: m0=integral S_r d omega, RMS=sqrt(m0), significant single amplitude=2 sqrt(m0). The latter is half the corresponding significant double amplitude; the highest-third interpretation requires narrow-band Gaussian peaks.</p>
<p>The modified Pierson–Moskowitz formula is S=(5/16) Hs² omega_p⁴ omega⁻⁵ exp[-(5/4)(omega_p/omega)⁴]. JONSWAP multiplies this shape by gamma raised to exp[-(omega/omega_p-1)²/(2 sigma²)], with sigma=7/100 below and 9/100 above the peak. The source is <cite>Waves in Ocean Engineering (1991)</cite>, pp107–108, identified and reproduced in the public Orcina wave-spectrum documentation. The separate parameter page confirms the two widths. The textbook itself was not accessed.</p>
<p>The synthetic heave-like curve is explicitly defined as R=peak_gain*exp[-((omega-omega_p)/peak_width)²]. It is an illustrative peak, with no mass, damping or hydrodynamic excitation model.</p>
<p>For hydrostatics, WAMIT User Manual v7.5 section 3.1 gives C33=rho*g*A, C44=rho*g*Ixx+rho*g*V*zB-m*g*zG, and the corresponding Iyy pitch term. With equilibrium mass rho*V, these reduce to the centered-waterplane equations checked by the adapter tests. Density and gravity are required inputs.</p>
<ul>{links}</ul></section>
<section id="worked-example"><h2>5 Worked example</h2>
<p>For the regular case, all phases are zero. At the wave crest, x=R*a={n('constant.amplitude')} m. A zero operator produces zero motion and a unit operator reproduces the wave. Two components are summed before any amplitude is inferred.</p>{regular}
<p>For JONSWAP, first apply the unit operator, then the synthetic peak. All plotted ordinates below are generated from the public calculation results.</p>{rao}{spectra}
<table><thead><tr><th>Case</th><th>m0 (m²)</th><th>RMS (m)</th><th>Significant single amplitude (m)</th></tr></thead><tbody>{result_rows}</tbody></table></section>
<section id="evidence"><h2>6 Numerical evidence and tail check</h2>
<p>The hand result Hs²/16 is {n('tail.infinite_m0')} m². A separate raw Pierson–Moskowitz curve is integrated without normalization. Substitution u=(omega_p/omega)⁴ gives m0(0,W)=Hs²/16 exp[-(5/4)(omega_p/W)⁴]. The full grid loses less than or equal to {n('tail.wide_loss_bound_pct')} % of infinite-band variance; the numerical relative tolerance is {n('tail.m0_relative_tolerance')}, bounded from the analytic tail and quadrature error in the value-test docstring.</p>
<p>Truncating that same raw curve at {n('tail.cutoff')} rad/s loses {n('tail.truncated_loss_pct')} % of variance. This diagnostic addresses the tail concern in #2613; no existing generator defaults are changed. Zeroth-moment convergence does not establish convergence of higher moments or zero-crossing period.</p>
<p>Tests check the peak-response integral by separate scalar Simpson quadrature, the constant case by hand arithmetic, the raw Pierson–Moskowitz integral against its antiderivative and hydrostatics against a tracked unit-box fixture. They fail on missing required files and never silently skip the new value checks.</p></section>
<section id="contracts"><h2>7 Input and output contracts</h2>
<p>Angular-frequency axes are rad/s and ascend strictly. Frequency axes are Hz and period axes seconds; periods descend to preserve increasing omega. Omega=2*pi*f=2*pi/T. Regular-wave phase is explicitly degrees or radians, independent of rotational amplitude units. Positive lag delays the response crest. Translation output is m for m/m RAOs; deg/m and rad/m RAOs retain their respective response units.</p>
<p>Spectral density is one-sided per rad/s. Transform density with its Jacobian before converting from Hz or period axes. A complex RAO enters through its squared magnitude. Non-monotonic grids, duplicate points, mismatched lengths, nonfinite values and negative density raise ValueError. No sorting, interpolation, extrapolation or default phase inference is performed.</p></section>
<section id="limits"><h2>8 Limits of use and NOT_EVALUATED</h2>
<ul><li>Geometry-specific RAOs, heading coupling, nonlinear response and forward speed: NOT_EVALUATED.</li><li>Licensed solver execution, native solver import, physical benchmarking and native export-schema acceptance: NOT_EVALUATED.</li><li>Extreme response, peak-distribution fit, higher spectral moments and operating limits: NOT_EVALUATED.</li><li>Hydrostatic adapter record: centered symmetric waterplane, zero horizontal centre offsets, equilibrium mass rho*V; finite heel, off-centre coupling and free-surface correction: NOT_EVALUATED.</li><li>The optional hydrostatic mapping is a separate SI calculation record. It does not qualify legacy vessel defaults or their native-unit conversion.</li></ul></section>
<section id="recommendations"><h2>9 Way forward</h2>
<p>Supply qualified complex RAOs with an explicit phase origin, response units and axis convention. Extend and refine the frequency grid for every statistic used. Confirm the narrow-band assumption before interpreting the significant single amplitude as a highest-third peak measure. Obtain a separate native-schema and solver comparison before transferring these records to an operational model.</p>
<p>Rebuild with <code>PYTHONPATH=src python scripts/reporting/build_rao_analysis_report.py</code>. Use <code>--output</code> to select an external rebuild directory. Run <code>python -m pytest tests/hydrodynamics/rao_analysis/test_module_report.py</code> with the source import path and declared dependencies. Numerical tests use unrounded computed values; displayed quantities use one decimal place.</p></section>
<section id="appendix"><h2>10 Reproduction evidence and symbols</h2>
<p>R is response per wave amplitude; a is wave amplitude; phi is response phase; theta is wave phase; omega is angular frequency; S is one-sided spectral density; m0 is variance; rho is density; g is gravity; A is waterplane area; Ixx and Iyy are waterplane second moments; V is displaced volume; zB and zG are vertical buoyancy and gravity centres.</p>
<p>All figures are generated from attached computed arrays. Downsampling affects presentation only. Rebuilding requires NumPy, SciPy and Pydantic because of the existing hydrodynamics package imports. This HTML has no remote presentation dependency. Source digests identify the calculation and report inputs; numerical libraries can introduce round-off differences across platforms.</p>
<table><thead><tr><th>Source</th><th>SHA-256</th></tr></thead><tbody>{digests}</tbody></table>
<details><summary>Changes</summary><ul>{change_list}</ul></details>
</section>
<script type="application/json" id="calculation">{json.dumps(data, sort_keys=True, separators=(',', ':'), allow_nan=False)}</script>
</main></body></html>
'''


def build(output):
    """Write only the two reproducible report deliverables."""
    changes_text = (OUTPUT/'changes.json').read_text()
    changes = json.loads(changes_text)
    output.mkdir(parents=True, exist_ok=True)
    (output/'report.html').write_text(render(calculate(), changes), encoding='utf-8')
    (output/'changes.json').write_text(changes_text, encoding='utf-8')


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--output', type=Path, default=OUTPUT)
    build(parser.parse_args().output)
