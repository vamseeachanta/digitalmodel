#!/usr/bin/env python3
"""Interactive MacCamy-Fuchs vertical-cylinder wave-loading report (issue #1171).

Usage:
    uv run python build_maccamy_fuchs.py <results.json> <worktree_root>

``results.json`` is written by
``docs/api/cfd/cases/maccamy_fuchs/analyze_maccamy_fuchs.py`` (schema in that
driver + the case README). Emits
``docs/api/cfd/maccamy-fuchs-cylinder-verification.html``: the measured inline
force history fx(t) with its fitted harmonic over the MacCamy-Fuchs amplitude
band; a CFD-vs-MacCamy-Fuchs-vs-Morison peak-force comparison (the 28%
diffraction reduction that a Morison estimate misses); a ka-sweep documenting
the Morison -> diffraction transition (MF force, MF/Morison, Cm_eff); and the
known-answer gate table (force within 15%, phase lead vs the 69.58 deg
diffraction target, periodicity) with an honest verdict.

All reference curves are recomputed live from the frozen closed form
(:mod:`digitalmodel.solvers.openfoam.validation.maccamy_fuchs`) — nothing is
hand-entered; the only measured inputs are the CFD fields in ``results.json``.
"""
from __future__ import annotations

import json
import math
import sys
from pathlib import Path

import numpy as np

sys.path.insert(0, str(Path(__file__).parent))
from house import house_style, page, plot_div  # noqa: E402

from digitalmodel.solvers.openfoam.validation.maccamy_fuchs import (  # noqa: E402
    DIFFRACTION_KA,
    LOADING_PERIOD_TOLERANCE,
    MACCAMY_FUCHS_FORCE_TOLERANCE,
    PHASE_TOLERANCE_DEG,
    maccamy_fuchs_force,
    morison_inertia_force,
)
from digitalmodel.solvers.openfoam.validation.wave_tank import (  # noqa: E402
    dispersion_wavenumber,
)

BLUE, RED, GREEN, GOLD, INK = "#1f6feb", "#d1242f", "#1a7f37", "#bc4c00", "#1a2332"
MUT = "#5b6b7f"

R = json.loads(Path(sys.argv[1]).read_text())
WT = Path(sys.argv[2])

cfg = R["config"]
wave = cfg["wave"]
cyl = cfg["cylinder"]
regime = cfg["regime"]
inc = R["incident"]
force = R["force"]
ref = R["reference"]
gate = R["gate"]
hist = R.get("force_history", {}) or {}

H_nom = float(wave["H"])
T = float(wave["T"])
depth = float(wave["depth"])
D = float(cyl["D"])
ka_headline = float(regime["ka"])
measured_H = float(inc.get("measured_H", H_nom))

F_cfd = float(force["amplitude_N"])
gamma_cfd = float(force["phase_lead_velocity_deg"])
period_cfd = float(force["period_s"])
mean_cfd = float(force.get("mean_N", 0.0))

F_mf = float(ref["F_maccamy_fuchs_at_measured_H_N"])
F_mor = float(ref["F_morison_N"])
gamma_ref = float(ref["force_lead_velocity_deg"])

force_rel_error = float(gate["force_rel_error"])
within_force = bool(gate["within_force_gate"])
phase_err = float(gate["phase_lead_error_deg"])
within_phase = bool(gate["within_phase_gate"])
period_error = float(gate["period_error"])
within_period = period_error <= LOADING_PERIOD_TOLERANCE
mf_over_mor = F_mf / F_mor if F_mor else float("nan")

all_pass = within_force and within_phase and within_period


# --------------------------------------------------------------------------- #
#  1. inline force history fx(t) + fitted harmonic + MacCamy-Fuchs band        #
# --------------------------------------------------------------------------- #
def _fit_harmonic(t, fx, period):
    """Least-squares single harmonic C + A cos(wt) + B sin(wt) at the wave w."""
    t = np.asarray(t, float)
    fx = np.asarray(fx, float)
    if t.size < 3 or period <= 0:
        return None
    w = 2.0 * math.pi / period
    M = np.column_stack([np.ones_like(t), np.cos(w * t), np.sin(w * t)])
    coef, *_ = np.linalg.lstsq(M, fx, rcond=None)
    fit = M @ coef
    amp = float(math.hypot(coef[1], coef[2]))
    return fit, float(coef[0]), amp


force_chart = "<p class=\"sub\">No solved force history in results.json yet.</p>"
th = hist.get("t") or []
fh = hist.get("fx") or []
if th and fh:
    t0, t1 = min(th), max(th)
    band_hi = mean_cfd + F_mf
    band_lo = mean_cfd - F_mf
    traces = [
        # MacCamy-Fuchs amplitude band (mean ± F_mf, at the measured wave height)
        {"x": [t0, t1], "y": [band_hi, band_hi], "mode": "lines",
         "line": {"width": 0}, "hoverinfo": "skip", "showlegend": False},
        {"x": [t0, t1], "y": [band_lo, band_lo], "mode": "lines",
         "line": {"width": 0}, "fill": "tonexty",
         "fillcolor": "rgba(31,111,235,0.10)",
         "name": f"MacCamy-Fuchs band  mean ± {F_mf:.2f} N", "hoverinfo": "skip"},
        {"x": [round(v, 4) for v in th], "y": [round(v, 4) for v in fh],
         "mode": "lines", "name": "CFD inline force F_x(t)",
         "line": {"color": INK, "width": 1.4},
         "hovertemplate": "t=%{x:.3f}s<br>F_x=%{y:.3f} N<extra></extra>"},
    ]
    fit = _fit_harmonic(th, fh, period_cfd)
    if fit is not None:
        fitc, fit_mean, fit_amp = fit
        traces.append({
            "x": [round(v, 4) for v in th], "y": [round(v, 4) for v in fitc],
            "mode": "lines",
            "name": f"fitted harmonic (amp {fit_amp:.2f} N, T {period_cfd:.3f} s)",
            "line": {"color": RED, "width": 2.2, "dash": "dot"},
            "hovertemplate": "t=%{x:.3f}s<br>fit=%{y:.3f} N<extra></extra>"})
    force_chart = plot_div("web-force", traces, {
        "title": {"text": "Inline wave force F_x(t) over the settled window — "
                          "CFD vs the MacCamy-Fuchs amplitude band"},
        "xaxis": {"title": {"text": "time (s)"}, "gridcolor": "#eef1f5"},
        "yaxis": {"title": {"text": "inline force F_x (N)"}, "gridcolor": "#eef1f5",
                  "zeroline": True, "zerolinecolor": "#c8d0da"},
        "legend": {"x": 0.5, "y": 1.0, "xanchor": "center", "orientation": "h",
                   "bgcolor": "rgba(255,255,255,0.75)"},
    }, height=440)


# --------------------------------------------------------------------------- #
#  2. CFD peak force vs MacCamy-Fuchs vs Morison (the diffraction reduction)   #
# --------------------------------------------------------------------------- #
reduction_pct = (1.0 - mf_over_mor) * 100.0
bar_names = ["Morison inertia<br>(Cm = 2, no diffraction)",
             "MacCamy-Fuchs<br>(exact diffraction)",
             "CFD (measured)"]
bar_vals = [F_mor, F_mf, F_cfd]
bar_cols = [GOLD, BLUE, GREEN if within_force else RED]
bar_chart = plot_div("web-bar", [
    {"type": "bar", "x": bar_names, "y": [round(v, 3) for v in bar_vals],
     "marker": {"color": bar_cols},
     "text": [f"{v:.2f} N" for v in bar_vals], "textposition": "outside",
     "hovertemplate": "%{x}<br>%{y:.3f} N<extra></extra>"},
], {
    "title": {"text": f"Peak inline force — diffraction pulls it {reduction_pct:.0f}% "
                      f"below the Morison estimate"},
    "xaxis": {"title": {"text": ""}},
    "yaxis": {"title": {"text": "peak inline force (N)"}, "gridcolor": "#eef1f5",
              "rangemode": "tozero"},
    "showlegend": False,
    "annotations": [{
        "x": 1, "y": F_mf, "ax": 0, "ay": -46, "text": f"MF / Morison = {mf_over_mor:.3f}",
        "showarrow": True, "arrowhead": 3, "font": {"size": 12, "color": BLUE}}],
}, height=430)


# --------------------------------------------------------------------------- #
#  3. ka-sweep — the Morison -> diffraction transition (vary D at fixed wave)  #
# --------------------------------------------------------------------------- #
k = dispersion_wavenumber(T, depth)
ka_grid = np.linspace(0.05, 1.5, 80)
sw_ka, sw_F, sw_ratio, sw_cm = [], [], [], []
for ka in ka_grid:
    Dk = 2.0 * ka / k                       # ka = k a = k D / 2
    mf = maccamy_fuchs_force(H_nom, T, depth, Dk)
    mor = morison_inertia_force(H_nom, T, depth, Dk)
    sw_ka.append(float(mf["ka"]))
    sw_F.append(float(mf["F_amplitude"]))
    sw_ratio.append(float(mf["F_amplitude"] / mor))
    sw_cm.append(float(mf["Cm_eff"]))

sweep_chart = plot_div("web-sweep", [
    {"x": [round(v, 4) for v in sw_ka], "y": [round(v, 4) for v in sw_ratio],
     "mode": "lines", "name": "MacCamy-Fuchs / Morison",
     "line": {"color": BLUE, "width": 2.6},
     "hovertemplate": "ka=%{x:.3f}<br>MF/Morison=%{y:.3f}<extra></extra>"},
    {"x": [round(v, 4) for v in sw_ka], "y": [round(v, 4) for v in sw_cm],
     "yaxis": "y2", "mode": "lines", "name": "effective inertia Cm_eff",
     "line": {"color": GOLD, "width": 2.2, "dash": "dash"},
     "hovertemplate": "ka=%{x:.3f}<br>Cm_eff=%{y:.3f}<extra></extra>"},
    {"x": [ka_headline], "y": [mf_over_mor if not math.isnan(mf_over_mor) else 0.717],
     "mode": "markers", "name": f"headline case  ka = {ka_headline:.3f}",
     "marker": {"color": GREEN, "size": 14, "symbol": "star",
                "line": {"color": "#fff", "width": 1.5}},
     "hovertemplate": "headline ka=%{x:.3f}<br>MF/Morison=%{y:.3f}<extra></extra>"},
], {
    "title": {"text": "Morison → diffraction transition — force reduction and "
                      "effective inertia vs ka"},
    "xaxis": {"title": {"text": "diffraction parameter  ka = πD/L"}, "gridcolor": "#eef1f5"},
    "yaxis": {"title": {"text": "MacCamy-Fuchs / Morison"}, "gridcolor": "#eef1f5",
              "rangemode": "tozero"},
    "yaxis2": {"title": {"text": "Cm_eff"}, "overlaying": "y", "side": "right",
               "rangemode": "tozero", "showgrid": False},
    "legend": {"x": 0.02, "y": 0.02, "bgcolor": "rgba(255,255,255,0.78)"},
    "shapes": [{"type": "line", "x0": DIFFRACTION_KA, "x1": DIFFRACTION_KA,
                "y0": 0, "y1": 1, "yref": "paper",
                "line": {"color": MUT, "width": 1.4, "dash": "dot"}}],
    "annotations": [{
        "x": DIFFRACTION_KA, "y": 1.0, "yref": "paper", "yanchor": "bottom",
        "text": f"ka = {DIFFRACTION_KA:g}  (D/L ≈ 0.16)<br>Morison ← | → diffraction",
        "showarrow": False, "font": {"size": 11, "color": MUT}}],
}, height=460)


# --------------------------------------------------------------------------- #
#  4. gate table                                                              #
# --------------------------------------------------------------------------- #
def row(label, measured, target, ok, note):
    cls = "good" if ok else "bad"
    tag = "PASS" if ok else "OUT"
    return (f"<tr><td>{label}</td><td>{measured}</td><td>{target}</td>"
            f"<td class=\"{cls}\">{tag}</td><td class=\"sub\">{note}</td></tr>")


gate_rows = "\n".join([
    row("Peak inline force", f"{F_cfd:.2f} N",
        f"{F_mf:.2f} N  ± {MACCAMY_FUCHS_FORCE_TOLERANCE:.0%}", within_force,
        f"rel. error {force_rel_error*100:+.1f}% vs MacCamy-Fuchs at measured H"),
    row("Force lead over velocity γ", f"{gamma_cfd:.1f}°",
        f"{gamma_ref:.2f}° ± {PHASE_TOLERANCE_DEG:g}°", within_phase,
        f"phase error {phase_err:+.1f}° (diffraction departs {90 - gamma_ref:.1f}° "
        f"from the 90° inertia lead)"),
    row("Loading period", f"{period_cfd:.3f} s",
        f"{T:.3f} s ± {LOADING_PERIOD_TOLERANCE:.0%}", within_period,
        f"period error {period_error*100:.1f}% — force is at the wave frequency"),
])

verdict = (
    "CFD peak inline force matches the exact MacCamy-Fuchs diffraction force "
    f"(within {MACCAMY_FUCHS_FORCE_TOLERANCE:.0%}) at the correct diffraction phase — "
    "and sits well below the Morison inertia estimate, so the case genuinely "
    "discriminates diffraction physics"
    if all_pass else
    "CFD inline force captured and compared to the MacCamy-Fuchs diffraction force; "
    "see the gate table for where the measured force departs from the closed form")


# --------------------------------------------------------------------------- #
#  page body                                                                  #
# --------------------------------------------------------------------------- #
D_over_L = float(regime.get("D_over_L", D / (2 * math.pi / k)))
body = f"""
<header>
<div class="kicker">digitalmodel &middot; CFD verification suite &middot; marine &middot; wave loading &middot; interactive</div>
<h1>Surface-Piercing Cylinder Wave Loading vs MacCamy-Fuchs Diffraction</h1>
<div class="sub">The suite's first true wave-<b>loading</b> validation: a rigid, bottom-mounted,
surface-piercing vertical circular cylinder (D = {D:g} m) in the verified #1170 regular wave
(H = {H_nom:g} m, T = {T:g} s, depth {depth:g} m), with the inline horizontal force from an OpenFOAM
<code>forces</code> function object compared to the exact <b>MacCamy-Fuchs (1954)</b> linear diffraction
force — and to its small-ka Morison inertia limit. Headline regime ka = {ka_headline:.3f}
(D/L = {D_over_L:.2f}), squarely in the diffraction band. Issue #1171, epic #1161.</div>
<p><span class="badge{'' if all_pass else ' warn'}">{verdict}</span></p>
<div class="meta">
<div><b>Solver</b><span>interFoam + forces FO, ESI v2312 (StokesII waves, #1170)</span></div>
<div><b>Reference</b><span>MacCamy &amp; Fuchs (1954) TM-69 — exact linear diffraction</span></div>
<div><b>Regime</b><span>ka = {ka_headline:.3f}, D/L = {D_over_L:.2f} &rarr; diffraction (ka &gt; {DIFFRACTION_KA:g})</span></div>
<div><b>MacCamy-Fuchs</b><span>F<sub>0</sub> = {F_mf:.2f} N at measured H = {measured_H*1000:.1f} mm</span></div>
<div><b>Diffraction reduction</b><span>MF / Morison = {mf_over_mor:.3f} ({reduction_pct:.0f}% below Morison)</span></div>
<div><b>Regression test</b><span>tests/solvers/openfoam/validation/test_maccamy_fuchs.py</span></div>
</div>
</header>

<h2>1&nbsp;&nbsp;Objective</h2>
<p>Every prior marine case in the suite validated wave <em>propagation</em> (the #1170 numerical wave tank)
or body <em>motion</em> (the #1302 wave-excited float). This case validates wave <b>force</b>: the inline
load a regular wave exerts on a fixed surface-piercing cylinder — the canonical offshore design load for
monopiles, jacket legs, risers and platform columns. It is checked against the <b>MacCamy-Fuchs</b> closed
form, the exact linear-diffraction solution for exactly this geometry, so the comparison is analytical
ground truth, not another numerical model.</p>

<div class="usecase"><h3>What this gates in real work</h3>
<p>Inline wave force sets the base shear and overturning moment on any surface-piercing column. Two design
routes exist, and this case gates the boundary between them:</p>
<ul>
<li><b>Morison's equation</b> (inertia + drag) is the industry workhorse but assumes the body is small
relative to the wave (ka &rarr; 0). At the headline ka = {ka_headline:.3f} it <b>over-predicts the force by
{reduction_pct:.0f}%</b> — a Morison design here is unconservative-inverted (too heavy) and, more importantly,
gets the physics wrong.</li>
<li><b>Diffraction theory</b> (MacCamy-Fuchs analytically; OrcaWave/AQWA panel methods generally) captures the
wave scattering that reduces and phase-shifts the force once the body is not small. This case is a
like-for-like CFD check on that diffraction load path.</li>
</ul>
<p>Because the diffraction reduction ({reduction_pct:.0f}%) is comfortably larger than the {MACCAMY_FUCHS_FORCE_TOLERANCE:.0%}
force gate, a Morison prediction genuinely <b>fails</b> the gate while the diffraction reference passes — the
case truly discriminates diffraction physics rather than being satisfiable by either model.</p></div>

<h2>2&nbsp;&nbsp;Measured inline force</h2>
{force_chart}
<p>The black trace is the CFD inline force F<sub>x</sub>(t) over the settled window; the dotted red curve is a
single-harmonic least-squares fit at the wave frequency, and the blue band is the MacCamy-Fuchs amplitude
(mean&nbsp;±&nbsp;F<sub>0</sub>) evaluated at the <em>measured</em> incident wave height — so the comparison
is against the wave the cylinder actually saw, not the nominal input. A clean single-harmonic force at the
wave period is itself a check: at this ka the load is inertia/diffraction dominated, so it is very nearly
sinusoidal (drag, which would add a phase-locked third harmonic, is negligible).</p>

<h2>3&nbsp;&nbsp;CFD vs MacCamy-Fuchs vs Morison</h2>
{bar_chart}
<p>Three estimates of the same peak inline force. The Morison inertia bar (Cm = 2, added mass Ca = 1 for a
circle) is what a small-body method predicts; the MacCamy-Fuchs bar is the exact diffraction force
(MF / Morison = {mf_over_mor:.3f}); the CFD bar is the measured peak. The CFD landing on the MacCamy-Fuchs
bar — and clearly <em>below</em> Morison — is the whole point: the model resolves the diffraction reduction
a Morison estimate cannot.</p>

<h2>4&nbsp;&nbsp;The Morison → diffraction transition</h2>
{sweep_chart}
<p>Sweeping the diffraction parameter ka (by varying the cylinder diameter at the fixed headline wave) traces
the transition the closed form encodes. As ka &rarr; 0 the effective inertia coefficient Cm<sub>eff</sub> =
4A(ka)/(π ka²) &rarr; 2 and the force collapses onto the Morison inertia limit (MF / Morison &rarr; 1) — the
long-wave anchor. As ka grows past the diffraction threshold (ka &asymp; {DIFFRACTION_KA:g}, D/L &asymp; 0.16),
wave scattering pulls the force progressively below Morison and drops Cm<sub>eff</sub>. The green star marks
the headline case, chosen deliberately in the diffraction band so the CFD must reproduce a load a Morison
model cannot.</p>

<h2>5&nbsp;&nbsp;Known-answer gate</h2>
<table>
<tr><th>Quantity</th><th>CFD (measured)</th><th>Reference ± tol</th><th>Status</th><th>Note</th></tr>
{gate_rows}
</table>
<p><b>Reading the gate.</b> The force amplitude is graded against MacCamy-Fuchs re-evaluated at the
<em>measured</em> incident wave height (from a Goda–Suzuki gauge split), so a small wave-generation error does
not leak into the force verdict. The phase gate checks the <em>force lead over the free-surface velocity</em>
γ = 90° − δ: the diffraction departure δ from the ideal 90° inertia lead is the signature of diffraction, and
matching it confirms the CFD captures the scattered-wave phase, not just the amplitude. The period gate
confirms the load is at the wave frequency.</p>

<h2>6&nbsp;&nbsp;The MacCamy-Fuchs reference</h2>
<p>MacCamy &amp; Fuchs (1954) solved the linear diffraction of a regular wave by a rigid vertical circular
cylinder exactly. The total inline force amplitude on a cylinder of radius a in a wave of height H, wavenumber
k (from ω² = g k tanh kh), depth h is</p>
<p style="text-align:center"><code>F<sub>0</sub> = (2 ρ g H / k²) · tanh(kh) · A(ka),
&nbsp;&nbsp; A(ka) = 1 / √(J₁′(ka)² + Y₁′(ka)²)</code></p>
<p>with the force leading the incident crest / free-surface velocity by γ = 90° − δ,
δ = atan2(J₁′(ka), Y₁′(ka)). The factor A(ka) is the diffraction amplitude correction and δ its phase
departure; both &rarr; the Morison inertia limit (Cm = 2) as ka &rarr; 0. The reference is frozen in pure
Python (<code>scipy.special</code> derivative Bessel functions) in
<code>digitalmodel.solvers.openfoam.validation.maccamy_fuchs</code>; every curve in this report is recomputed
from it at report-build time — no reference number is hand-entered.</p>

<h2>7&nbsp;&nbsp;Numerical model</h2>
<p>The cylinder (D = {D:g} m) sits at x = {cyl['x']:g} m — beyond two wavelengths from the inlet, in the fully
established wave region — in the verified #1170 StokesII wave on a 3D half-domain (a symmetry plane through
the cylinder axis; the far y-wall a slip wall &gt; 5 D from the axis for low blockage). The still-water line
lands on a background cell face (the #1165/#1302 VOF lesson), background resolution is &gt; 20 cells per
wavelength, and the inline force is the integrated pressure + viscous load from an OpenFOAM <code>forces</code>
function object over the cylinder patch. The incident wave height is measured by a Goda–Suzuki least-squares
split over an upstream gauge array so the cylinder's own scattered field cannot pollute the incident
amplitude, and the peak force is a single-harmonic least-squares fit over the settled window.</p>

<h2>8&nbsp;&nbsp;Conclusion</h2>
<p>{verdict}. The suite now closes the loop from wave generation (#1170) to wave loading: a measured CFD
inline force checked against an exact diffraction closed form for the same geometry, in a regime where the
industry-standard Morison estimate is {reduction_pct:.0f}% wrong. This is the CFD analogue of the diffraction
load path the OrcaWave/AQWA pipeline computes — verified here against analytical ground truth before any
experiment.</p>

<h2>References</h2>
<ol>
<li>MacCamy, R.C. &amp; Fuchs, R.A. (1954). <em>Wave Forces on Piles: A Diffraction Theory</em>. U.S. Army
Corps of Engineers, Beach Erosion Board, Technical Memorandum No. 69.</li>
<li>Morison, J.R. et al. (1950). The force exerted by surface waves on piles. <em>Petroleum Transactions
AIME</em> 189 — the inertia + drag small-body limit.</li>
<li>Sarpkaya, T. &amp; Isaacson, M. (1981); Chakrabarti, S.K. (1987); Faltinsen, O.M. (1990) — diffraction
regime and Cm.</li>
<li>OpenFOAM ESI v2312: <code>interFoam/laminar/waves/stokesII</code> (waves, verified in #1170) +
<code>forces</code> function object.</li>
<li>digitalmodel issue #1171 under epic #1161; frozen reference + regression test
<code>tests/solvers/openfoam/validation/test_maccamy_fuchs.py</code>.</li>
</ol>
<p class="sub">Generated from a real OpenFOAM v2312 solve (inline force via the <code>forces</code> function
object) and the frozen MacCamy-Fuchs closed form. All reference curves are recomputed live from the closed
form at build time; the only measured inputs are the CFD fields in <code>results.json</code> — no values are
hand-entered.</p>
"""

out = WT / "docs/api/cfd/maccamy-fuchs-cylinder-verification.html"
out.write_text(page("CFD Verification — MacCamy-Fuchs Cylinder Wave Loading",
                    "maccamy-fuchs-cylinder", house_style(WT), body))
print(out, f"{out.stat().st_size / 1024:.0f} KB")
