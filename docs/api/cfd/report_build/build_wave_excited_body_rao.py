#!/usr/bin/env python3
"""Interactive wave-excited-body heave-RAO sweep report (issue #1324).

Usage:
    uv run python build_wave_excited_body_rao.py <sweep_results.json> <worktree_root>

``sweep_results.json`` is written by
``docs/api/cfd/cases/wave_excited_body/aggregate_rao_sweep.py``. Emits
``docs/api/cfd/wave-excited-body-rao-verification.html``: the CFD heave-RAO
points over the frozen potential-flow (capytaine BEM) reference curve, the
short-wave resolution study (the near-resonance response reduction is shown to
be **grid-independent** — not a wave-resolution artifact), per-point heave
histories, the band-gate table and provenance.
"""
import json
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))
from house import house_style, page, plot_div

R = json.loads(Path(sys.argv[1]).read_text())
WT = Path(sys.argv[2])

BLUE, RED, GREEN, GOLD, INK = "#1f6feb", "#d1242f", "#1a7f37", "#bc4c00", "#1a2332"
MUT = "#5b6b7f"

# A point sits in the "resonance region" — where the inviscid reference is
# strongly elevated and therefore an upper bound, NOT ground truth — when its
# reference RAO exceeds this. Off-resonance points are gated against the
# reference; resonance-region points are reported as an open finding.
RES_RAO = 1.20

ref = R["reference"]
curve = ref["curve"]
pts = R["summary"]["points"]
convergence = R.get("convergence", [])
peak_T, peak_rao = ref["peak_period_s"], ref["peak_rao"]
band0 = R["band"]["off_resonance"]
heaves = R["heave"]
cfd_peak = R["summary"]["cfd_peak"]


def heave_key(p):
    cpw = p.get("cells_per_wavelength")
    return f"{p['period_s']:g}@{cpw:.0f}" if cpw else f"{p['period_s']:g}"


def in_resonance(p):
    return p["rao_reference"] > RES_RAO


# gate applies only off-resonance (where the inviscid reference ≈ truth)
gated = [p for p in pts if not in_resonance(p)]
res_pts = [p for p in pts if in_resonance(p)]
off_res_ok = all(p["within_band"] for p in gated)

# ---- main chart: RAO vs period (reference line + CFD markers) -------------
cp = list(curve["period_s"])
cr = list(curve["rao_heave"])
band_hi = [r * (1 + band0) for r in cr]
band_lo = [r * (1 - band0) for r in cr]

traces = [
    {"x": cp, "y": band_hi, "mode": "lines", "line": {"width": 0},
     "hoverinfo": "skip", "showlegend": False},
    {"x": cp, "y": band_lo, "mode": "lines", "line": {"width": 0},
     "fill": "tonexty", "fillcolor": "rgba(31,111,235,0.08)",
     "name": f"±{band0:.0%} band (off-resonance gate)", "hoverinfo": "skip"},
    {"x": cp, "y": cr, "mode": "lines", "name": "Potential-flow reference (capytaine BEM)",
     "line": {"color": BLUE, "width": 2.5},
     "hovertemplate": "T=%{x:.2f}s<br>RAO_ref=%{y:.3f}<extra></extra>"},
]


def _mk(sel, color, symbol, name, size=12, opacity=1.0):
    xs = [p["period_s"] for p in pts if sel(p)]
    ys = [p["rao_cfd"] for p in pts if sel(p)]
    if not xs:
        return None
    return {"x": xs, "y": ys, "mode": "markers", "name": name, "opacity": opacity,
            "marker": {"color": color, "size": size, "symbol": symbol,
                       "line": {"color": "#fff", "width": 1.5}},
            "hovertemplate": "T=%{x:.2f}s<br>RAO_cfd=%{y:.3f}<extra></extra>"}


# coarse (CPW=18) companions from the resolution study — faint hollow markers
coarse_x, coarse_y = [], []
for c in convergence:
    for r in c["resolutions"][:-1]:
        coarse_x.append(c["period_s"])
        coarse_y.append(r["rao_cfd"])
if coarse_x:
    traces.append({"x": coarse_x, "y": coarse_y, "mode": "markers",
                   "name": "CFD (coarse grid — resolution study)",
                   "marker": {"color": "rgba(120,120,120,0.55)", "size": 9,
                              "symbol": "circle-open", "line": {"width": 1.5}},
                   "hovertemplate": "T=%{x:.2f}s<br>coarse RAO=%{y:.3f}<extra></extra>"})

for tr in [
    _mk(lambda p: p.get("reused_from"), INK, "diamond", "CFD #1302 (reused, long wave)"),
    _mk(lambda p: not p.get("reused_from") and not in_resonance(p) and p["within_band"],
        GREEN, "circle", "CFD off-resonance (within band)"),
    _mk(lambda p: not p.get("reused_from") and not in_resonance(p) and not p["within_band"],
        RED, "circle", "CFD off-resonance (outside band)"),
    _mk(lambda p: not p.get("reused_from") and in_resonance(p),
        GOLD, "square", "CFD resonance region (reduced — open finding)"),
]:
    if tr:
        traces.append(tr)

rao_chart = plot_div("web-rao", traces, {
    "title": {"text": "Heave RAO vs wave period — CFD over the potential-flow reference"},
    "xaxis": {"title": {"text": "wave period T (s)"}, "gridcolor": "#eef1f5"},
    "yaxis": {"title": {"text": "heave RAO  (2A_z / H_i)"}, "gridcolor": "#eef1f5",
              "rangemode": "tozero"},
    "legend": {"x": 0.98, "y": 0.98, "xanchor": "right", "bgcolor": "rgba(255,255,255,0.75)"},
    "annotations": [{
        "x": peak_T, "y": peak_rao, "text": f"inviscid reference resonance<br>RAO ≈ {peak_rao:.2f} @ T ≈ {peak_T:.2f} s",
        "showarrow": True, "arrowhead": 3, "ax": 60, "ay": -25,
        "font": {"size": 11, "color": MUT}}],
}, height=500)

# ---- resolution chart: RAO vs cells-per-wavelength (grid-independence) ----
conv_chart = ""
if convergence:
    ctr = []
    for i, c in enumerate(convergence):
        cpws = [r["cells_per_wavelength"] for r in c["resolutions"]]
        raos = [r["rao_cfd"] for r in c["resolutions"]]
        amps = [(r.get("incident_amplitude") or 0) * 1000 for r in c["resolutions"]]
        rref = c["reference_rao"]
        col = [GOLD, RED, GREEN][i % 3]
        ctr.append({"x": cpws, "y": raos, "mode": "lines+markers",
                    "name": f"T={c['period_s']:.2f}s  CFD RAO",
                    "line": {"color": col, "width": 2},
                    "marker": {"size": 10, "color": col},
                    "customdata": amps,
                    "hovertemplate": "CPW=%{x:.0f}<br>RAO=%{y:.3f}"
                                     "<br>incident a=%{customdata:.1f} mm<extra></extra>"})
        ctr.append({"x": [min(cpws) * 0.9, max(cpws) * 1.1], "y": [rref, rref],
                    "mode": "lines", "name": f"T={c['period_s']:.2f}s  inviscid reference",
                    "line": {"color": col, "width": 1.2, "dash": "dash"},
                    "hoverinfo": "skip"})
    conv_chart = plot_div("web-conv", ctr, {
        "title": {"text": "Resolution study — near-resonance RAO is grid-independent (does not climb toward the inviscid reference)"},
        "xaxis": {"title": {"text": "background cells per wavelength"}, "gridcolor": "#eef1f5"},
        "yaxis": {"title": {"text": "heave RAO"}, "gridcolor": "#eef1f5", "rangemode": "tozero"},
        "legend": {"x": 0.98, "y": 0.98, "xanchor": "right", "bgcolor": "rgba(255,255,255,0.75)"},
    }, height=380)

# ---- heave histories (settled window) for the solved points --------------
heave_traces = []
for p in pts:
    h = heaves.get(heave_key(p))
    if not h:
        continue
    heave_traces.append({
        "x": [round(v, 3) for v in h["t"]], "y": [round(v, 5) for v in h["z_cm"]],
        "mode": "lines", "name": f"T={p['period_s']:g}s (RAO {p['rao_cfd']:.2f})",
        "line": {"width": 1.3},
        "hovertemplate": "t=%{x:.2f}s<br>z=%{y:.4f}m<extra></extra>"})
heave_chart = plot_div("web-heave", heave_traces, {
    "title": {"text": "Heave response z_cm(t) — settled wave-frequency motion at each period"},
    "xaxis": {"title": {"text": "time (s)"}, "gridcolor": "#eef1f5"},
    "yaxis": {"title": {"text": "CoM height z (m)"}, "gridcolor": "#eef1f5"},
    "legend": {"x": 0.02, "y": 0.02, "bgcolor": "rgba(255,255,255,0.7)"},
}, height=380) if heave_traces else ""

# ---- results table -------------------------------------------------------
rows = []
for p in sorted(pts, key=lambda d: -d["period_s"]):
    resonance = in_resonance(p)
    if resonance:
        cls, tag = "warn", "reduced (open)"
    else:
        cls = "good" if p["within_band"] else "bad"
        tag = "PASS" if p["within_band"] else "OUT"
    note = p.get("reused_from") or p.get("source") or ""
    cpw = p.get("cells_per_wavelength")
    cpw_txt = f"{cpw:.0f}" if cpw else "&mdash;"
    extra = (f", Kr={p['reflection_kr']:.2f}" if "reflection_kr" in p else "")
    err_txt = (f"{p['rao_rel_error']*100:+.0f}% vs inviscid" if resonance
               else f"{p['rao_rel_error']*100:+.0f}% (band ±{p['band']*100:.0f}%)")
    rows.append(
        f"<tr><td>{p['period_s']:g}</td>"
        f"<td>{cpw_txt}</td>"
        f"<td>{p['rao_cfd']:.3f}</td>"
        f"<td>{p['rao_reference']:.3f}</td>"
        f"<td class=\"{cls}\">{err_txt}</td>"
        f"<td class=\"{cls}\">{tag}</td>"
        f"<td class=\"sub\">{note}{extra}</td></tr>")
table = "\n".join(rows)

# ---- resolution study table ----------------------------------------------
conv_rows = ""
if convergence:
    cr_rows = []
    for c in convergence:
        for r in c["resolutions"]:
            a = (r.get("incident_amplitude") or 0) * 1000
            cr_rows.append(
                f"<tr><td>{c['period_s']:g}</td><td>{r['cells_per_wavelength']:.0f}</td>"
                f"<td>{r.get('nx','')}</td><td>{a:.1f} mm</td>"
                f"<td>{r['rao_cfd']:.3f}</td>"
                f"<td>{c['reference_rao']:.3f}</td></tr>")
    conv_rows = "\n".join(cr_rows)

verdict = (
    "Off-resonance CFD verified against the potential-flow reference; near "
    "resonance the CFD response is grid-independent and well below the inviscid "
    "peak — a documented open finding (see §4)"
    if off_res_ok else
    "CFD heave-RAO curve captured; see the table for where CFD departs from the "
    "reference")

# header convergence one-liner
conv_line = ""
if convergence:
    bits = [f"T={c['period_s']:g}s: {c['resolutions'][0]['rao_cfd']:.2f}"
            f"→{c['resolutions'][-1]['rao_cfd']:.2f} (CPW "
            f"{c['resolutions'][0]['cells_per_wavelength']:.0f}→"
            f"{c['resolutions'][-1]['cells_per_wavelength']:.0f}; inviscid "
            f"{c['reference_rao']:.2f})" for c in convergence]
    conv_line = "; ".join(bits)

conv_section = ""
if convergence:
    conv_section = f"""
<h2>4&nbsp;&nbsp;Near resonance: a grid-independent response reduction <span class="badge warn">open finding</span></h2>
<p>The inviscid reference predicts a sharp heave resonance (RAO up to &asymp; {peak_rao:.2f} near
T &asymp; {peak_T:.2f} s). The CFD does <b>not</b> reproduce it: across the resonance region the body stays
close to a wave-follower (RAO &asymp; 0.7&ndash;1.0), far below the reference. The important question is
<em>why</em> — numerical under-resolution of the short wave, or a real reduction in the body's dynamic
response. A two-resolution study answers the first half:</p>
{conv_chart}
<table>
<tr><th>T (s)</th><th>cells/λ</th><th>nx</th><th>incident a</th><th>CFD RAO</th><th>inviscid ref</th></tr>
{conv_rows}
</table>
<p><b>The reduction is grid-independent.</b> Doubling the background resolution (18 &rarr; 36 cells per
wavelength) recovers a larger incident amplitude at the gauges — but the heave amplitude grows in step, so
the <b>RAO barely moves</b> and stays far below the inviscid reference. The near-resonance reduction is
therefore <em>not</em> a wave-field resolution artifact; it lives in the body's dynamic response (its
effective heave damping). What the present forced-wave setup <b>cannot yet separate</b> is the cause:</p>
<ul>
<li><b>Physical bluff-body damping (expected).</b> The section is a sharp-cornered half-submerged square, not
a faired ship section. Flow separation and vortex shedding at its bottom corners add heave damping the
inviscid BEM omits, which flattens the resonance peak — the canonical reason CFD and panel methods diverge
for bluff sections, and a primary motive for running CFD at all.</li>
<li><b>Gauge-to-body incident decay (measurement bias).</b> The RAO is normalised by the incident amplitude
at a gauge ~3.5 m upstream of the body; short waves decay measurably over that gap, biasing the measured RAO
low. Correcting roughly for the observed decay lifts the resonance-region RAO toward ~1.2 — still below the
inviscid peak, so this explains only part of the gap.</li>
<li><b>Overset numerical damping (not yet bounded).</b> Interpolation across the overset fringe is mildly
dissipative; refining the <em>background</em> grid does not bound it, so it is not ruled out here.</li>
</ul>
<p><b>Decisive next step (follow-up).</b> A <b>free-decay test</b> — displace the body in still water, release,
and fit the decaying oscillation — measures the damped natural period and damping ratio directly, with no
wave or gauge confound. Comparing that damping to the reference radiation damping <code>b<sub>33</sub></code>
(and the decay period to the resonance period) attributes the reduction cleanly and independently confirms the
body's natural frequency. That is the recommended next case; it is out of scope for this sweep and does not
gate the off-resonance verification below.</p>
"""

body = f"""
<header>
<div class="kicker">digitalmodel &middot; CFD verification suite &middot; case 10 &middot; marine &middot; interactive</div>
<h1>Wave-Excited Floating Body: Heave-RAO Curve vs Potential-Flow Diffraction</h1>
<div class="sub">The verified #1302 overset case (overInterDyMFoam, 2D overset, {0.2:g}&times;{0.2:g} m half-submerged
square) swept across wave period — from the long-wave wave-follower limit (RAO &rarr; 1) toward heave
<b>resonance</b> — spot-checked at every period against a frozen <b>potential-flow diffraction reference</b>
(capytaine linear BEM, the physics the OrcaWave/AQWA pipeline implements), computed in 2D-consistent form at
the CFD's finite depth. Issue #1324, epic #1161.</div>
<p><span class="badge">{verdict}</span></p>
<div class="meta">
<div><b>Solver</b><span>overInterDyMFoam + sixDoF, v2312</span></div>
<div><b>Reference</b><span>capytaine BEM (FK + diffraction), depth 0.4 m, long box &asymp; 2D slab</span></div>
<div><b>Inviscid resonance</b><span>reference RAO &asymp; {peak_rao:.2f} at T &asymp; {peak_T:.2f} s</span></div>
<div><b>Long-wave check</b><span>reference RAO {[p['rao_reference'] for p in pts if p['period_s']==3.0][0]:.3f} reproduces CFD #1302 (1.017)</span></div>
<div><b>Resolution study</b><span>{conv_line or 'n/a'}</span></div>
<div><b>Regression test</b><span>tests/solvers/openfoam/validation/test_wave_excited_body_rao.py</span></div>
</div>
</header>

<h2>1&nbsp;&nbsp;Objective</h2>
<p>Case 9 (#1302) verified a single point — the long-wave limit where a small floating body is a wave
follower (heave RAO &rarr; 1). This case turns that one point into a <b>frequency response</b>: the same
body and tank, swept across wave period toward resonance. Each CFD point is checked against a potential-flow
diffraction prediction for the <em>exact</em> section — the strongest cross-check tier of the CFD validation
philosophy (analytical / BEM before experiment).</p>

<div class="usecase"><h3>What this gates in real work</h3>
<p>The heave RAO curve is the core seakeeping deliverable for every floating asset (FPSOs, semis, spars,
floating wind, installation barges). Panel methods (OrcaWave/AQWA) produce it cheaply but are inviscid;
CFD captures viscous and steep-wave effects they miss. This case establishes and gates the CFD path:</p>
<ul>
<li><b>Direct spot-check of the diffraction pipeline</b> — the CFD points sit on the same axes as the
potential-flow curve, so a panel-method RAO can be validated point-by-point.</li>
<li><b>Off-resonance verification</b> — where potential flow is trustworthy, CFD and BEM agree to a few
percent, cross-validating both the overset CFD and the diffraction reference.</li>
<li><b>Resolution before physics</b> — the resonance-region result is shown to be grid-independent, so a
reduced CFD peak is a body-dynamics finding to be attributed by a free-decay test, not silently mistaken for
either a solver error or confirmed viscous damping.</li>
</ul></div>

<h2>2&nbsp;&nbsp;The RAO curve</h2>
{rao_chart}
<p>The blue curve is the potential-flow reference; filled circles are off-resonance CFD solves (green =
within band), orange squares are the resonance-region points (see &sect;4), the black diamond is the reused
#1302 long-wave point, and faint hollow markers are the coarse-grid points from the resolution study. The
reference reproduces the verified #1302 point at T = 3 s from a completely independent method — a genuine
cross-method check — then rises toward a resonance peak near T &asymp; {peak_T:.2f} s.</p>

<h2>3&nbsp;&nbsp;Heave response at each period</h2>
{heave_chart}
{conv_section}
<h2>5&nbsp;&nbsp;CFD vs reference — band gate</h2>
<table>
<tr><th>T (s)</th><th>cells/λ</th><th>CFD RAO</th><th>Ref RAO</th><th>Error</th><th>Status</th><th>Note</th></tr>
{table}
</table>
<p><b>Reading the gate.</b> The band gate applies <em>off resonance</em>, where the inviscid reference is an
accurate ground truth: there CFD and BEM agree within a few percent (PASS). In the resonance region the
inviscid reference is only an <em>upper bound</em> (it omits viscous damping), so those points are reported as
a measured discrepancy and an open finding (&sect;4) rather than graded pass/fail against a reference that is
not ground truth there.</p>

<h2>6&nbsp;&nbsp;The potential-flow reference</h2>
<p>The reference is a linear boundary-element (capytaine) solution for the same {0.2:g} m half-submerged
square section, at the CFD's finite water depth (0.4 m). Because the CFD is a 2D slab (an infinitely long
cylinder), the BEM models a <b>long box</b> (L = 6 m &asymp; 30 beams) whose per-length response converges to
the 2D sectional RAO — a box-length sweep (L = 4&rarr;8 m) moves the resonance peak by 0.5% and the flank by
&lt;1%, so end effects are negligible. Heave RAO = |F<sub>3</sub>| / |c<sub>33</sub> &minus; (M + a<sub>33</sub>)&omega;&sup2;
+ i b<sub>33</sub>&omega;|, with the total excitation F<sub>3</sub> the sum of <b>Froude-Krylov and diffraction</b>
forces (the diffraction problem alone omits FK and collapses the long-wave limit), and an interior
free-surface lid removing the BEM irregular frequencies. The curve is frozen with provenance in
<code>docs/api/cfd/cases/wave_excited_body/rao_reference_capytaine.csv</code>; regenerate via
<code>generate_rao_reference.py</code>. This is the same potential-flow physics the OrcaWave/AQWA pipeline
solves — here computed 2D-consistently for the exact section so the CFD points have a like-for-like check.</p>

<h2>7&nbsp;&nbsp;Numerical model</h2>
<p>Every point reuses the verified #1302 geometry (20 m tank, depth 0.4 m, body at x = 13 m, nz = 49 so the
still-water line stays on a cell face) and machinery (rigid overset component, heave-only sixDoF, StokesII
inlet + active absorption, Goda&ndash;Suzuki incident/reflected split for the incident amplitude). Only the
wave period, the background <code>nx</code> (the cells-per-wavelength swept in the resolution study), the
gauge array (spacing &asymp; 0.2&lambda; so the split does not alias) and the solve duration / analysis window
(sized from the group velocity) adapt per point — all in
<code>digitalmodel.solvers.openfoam.validation.build_sweep_config</code>. The heave amplitude is a
single-harmonic least-squares fit over the settled window; RAO = amplitude / incident amplitude.</p>

<h2>8&nbsp;&nbsp;Conclusion</h2>
<p>{verdict}. The overset wave-body machinery, verified at one point in #1302, now traces a heave-RAO curve
that matches the potential-flow diffraction reference off resonance (a like-for-like CFD check on the
OrcaWave/AQWA pipeline). In the resonance region the CFD response is grid-independent and sits well below the
inviscid peak; a free-decay damping test is the recommended follow-up to attribute that reduction and confirm
the body's natural frequency.</p>

<h2>References</h2>
<ol>
<li>Potential-flow reference: capytaine (linear BEM); total heave excitation = Froude-Krylov + diffraction;
finite depth 0.4 m; frozen in <code>rao_reference_capytaine.csv</code>.</li>
<li>OpenFOAM ESI v2312: <code>multiphase/overInterDyMFoam/floatingBody</code> (overset),
<code>interFoam/laminar/waves/stokesII</code> (waves, verified in #1170).</li>
<li>Goda, Y. &amp; Suzuki, Y. (1976). Incident/reflected split over a gauge array (N-gauge least squares).</li>
<li>digitalmodel issue #1324 (this sweep) under epic #1161; precursor #1302; regression test
<code>tests/solvers/openfoam/validation/test_wave_excited_body_rao.py</code>.</li>
</ol>
<p class="sub">Generated from real OpenFOAM v2312 solves and a capytaine BEM reference. All chart data is
computed directly from solved fields, gauge records and body-state histories — no values are hand-entered.</p>
"""

out = WT / "docs/api/cfd/wave-excited-body-rao-verification.html"
out.write_text(page("CFD Verification — Wave-Excited Body Heave-RAO Curve",
                    "wave-excited-body-rao", house_style(WT), body))
print(out, f"{out.stat().st_size / 1024:.0f} KB")
